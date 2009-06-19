;; (C) 2009 Rémi Vanicat <vanicat@debian.org>

;; xmms2.el is a music player for Emacs that uses xmms2 as its backend

;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 3 of
;;     the License, or (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;     Boston, MA 02110-1301 USA

;;; Commentary

;; For know, only some command for simple control of xmms2.

;; you can use xmms2-status to know what is playing and
;; xmm2-play/xmm2-toggle/xmm2-pause/xmm2-stop/xmm2-next/xmm2-prev to
;; control xmms2

;; Installing :
;; put this file in some direcories in your load path
;; add (require 'xmms2) to you .emacs

;; you can also add keybinding: for example, for my multimedia keys:

;; (global-set-key [<XF86AudioPlay>] 'xmms2-toggle)
;; (global-set-key [<XF86Back>] 'xmms2-prev)
;; (global-set-key [<XF86Forward>] 'xmms2-next)

;;; TODO
;; a mode to view/edit playlists
;; a mode to view/edit collection
;; a mode to view current status
;; maybe status bar integration


(defvar xmms2-playlist nil
  "The xmms2 playlist,
Its format is
  (POS LIST)
where POS is the current position and LIST is a list of
  (ID ARTIST ALBUM TITLE URL)
Where ID is the xmms2 id of the song, and ARTIST ALBUM TITLE maybe be nil
if xmms2 doesn't know them")

(defvar xmms2-now-playing nil
  "The song that is being played now")

(defvar xmms2-status nil
  "The status of xmms2, a symbol
can be paused, stopped, playing or nil
nil mean that there is noconnection or there was an error")

(defvar xmms2-info-cache (make-hash-table :weakness 'value))

(defgroup xmms2 ()
  "xmms2.el is a client for xmms2 written in emacs lisp")

(defcustom xmms2-command
  "nyxmms2"
  "The command to run for xmms2 command. Must be API compatible with nyxmms2"
  :group 'xmms2
  :type 'string)

(defvar xmms2-process ())
(defvar xmms2-callback-queue ()
  "queue of callback to be run from the filter")

;;; some function to read anwser from nyxmms2

(defun xmms2-decode-info (info)
  (with-temp-buffer
    (insert info)
    (goto-char (point-min))
    (let (id title album artist url result)
      (search-forward-regexp "] id = \\([0-9]+\\)$")
      (setq id (string-to-number (match-string 1)))
      (goto-char (point-min))
      (search-forward-regexp "] url = \\([^\n]+\\)$")
      (setq url (match-string 1))
      (goto-char (point-min))
      (when (search-forward-regexp "] title = \\([^\n]+\\)$" () t)
	(setq title (match-string 1)))
      (goto-char (point-min))
      (when (search-forward-regexp "] album = \\([^\n]+\\)$" () t)
	(setq album (match-string 1)))
      (goto-char (point-min))
      (when (search-forward-regexp "] artist = \\([^\n]+\\)$" () t)
	(setq artist (match-string 1)))
      (setq result (list id artist album title url))
      (puthash id result xmms2-info-cache)
      result)))

(defun xmms2-decode-list (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (current-pos id result list)
      (while (search-forward-regexp "^\\(->\\|  \\)\\[\\([0-9]+\\)/\\([0-9]+\\)\\] \\(.*\\)$" () t)
	(when (string= (match-string 1) "->")
	  (setq current-pos (string-to-number (match-string 2))))
	(setq id (string-to-number (match-string 3)))
	(setq result (gethash id xmms2-info-cache (cons id (match-string 4))))
	(push result list))
      (cons current-pos (nreverse list)))))


;;; function for the connection itself

(defun xmms2-clean ()
  (when (processp xmms2-process)
    (with-current-buffer (process-buffer xmms2-process)
      (delete-region (point-min) (point-max))
      (setq xmms2-callback-queue ())
      (setq xmms2-process ()))))


(defun xmms2-process-sentinel (proc event)
  ;; Is this only called when the process died ?
  (xmms2-clean))

(defun xmms2-process-filter (proc str)
  (with-current-buffer (process-buffer xmms2-process)
    (goto-char (process-mark xmms2-process))
    (insert str)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (while (search-forward-regexp "xmms2> *$" () t)
      (let ((response (buffer-substring (point-min) (match-beginning 0))))
	(delete-region (point-min) (match-end 0))
	(apply (pop xmms2-callback-queue) (list response))))))

(defun xmms2-callback-message (response)
  (message response))

(defun xmms2-init-process ()		;TODO: add option for server and such
  (setq xmms2-process (start-process "nyxmms2" " *nyxmms2*" xmms2-command))
  (setq xmms2-callback-queue (append xmms2-callback-queue (list 'xmms2-callback-message)))
  (set-process-filter xmms2-process 'xmms2-process-filter)
  (set-process-sentinel xmms2-process 'xmms2-process-sentinel)
  (xmms2-status))

(defun xmms2-ensure-connected ()
  (unless (and xmms2-process
	       (eq (process-status xmms2-process)
		   'run))
    (xmms2-init-process)))

(defun xmms2-call (callback command &rest args)
  (xmms2-ensure-connected)
  (let ((string (format command args)))
    (process-send-string xmms2-process string)
    (process-send-string xmms2-process "\n")
    (setq xmms2-callback-queue (append xmms2-callback-queue (list callback)))))

;;; the commands

(defun xmms2-exit-process ()
  (xmms2-call 'ignore "exit"))

(defun xmms2-callback-current-info (response)
  (setq xmms2-now-playing (xmms2-decode-info response)))

(defun xmms2-callback-status (response)
  (unless (string-match "^\\(Paused\\|Stopped\\|Playing\\):" response)
    (setq xmms2-status ())
    (error "wrong status message"))
  (cond
    ((string= (match-string 1 response) "Playing")
     (setq xmms2-status 'playing))
    ((string= (match-string 1 response) "Paused")
     (setq xmms2-status 'paused))
    ((string= (match-string 1 response) "Stopped")
     (setq xmms2-status 'stopped)))
  (message response))

(defun xmms2-status ()
  (interactive)
  (xmms2-call 'xmms2-callback-status "status")
  (xmms2-call 'xmms2-callback-current-info "info"))

(defun xmms2-song-string (song)
  (if (listp (cdr song))
      (let ((artist (propertize (nth 1 song) 'face 'xmms2-artist))
	    (album (propertize (nth 2 song) 'face 'xmms2-album))
	    (title (propertize (nth 3 song) 'face 'xmms2-title)))
	(format "%s\n\tby %s from %s" title artist album))
      (propertize (cdr song) 'face 'xmms2-song)))



(defun xmms2-callback-list (response)
  (setq xmms2-playlist (xmms2-decode-list response))
  (with-current-buffer (xmms2-playlist-buffer)
    (let ((buffer-read-only ())
	  (current-point (point))
	  (num 1)
	  (song-string)
	  (current-marker)
	  (current (car xmms2-playlist)))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (cond ((eq xmms2-status 'playing) "Playing")
		    ((eq xmms2-status 'paused) "Paused")
		    ((eq xmms2-status 'stopped) "Stopped")
		    ('t "Unkown")))
      (insert "\n\n")

      (dolist (song (cdr xmms2-playlist))
	(setq song-string (xmms2-song-string song))
	(setq current-marker (if (= num current) "-> " "   "))
	(setq song-string (concat current-marker song-string "\n"))
	(setq song-string (propertize song-string
				      'xmms-num num
				      'xmms-id (car song)))
	(insert song-string)
	(setq num (1+ num)))
      (goto-char current-point))))

(defun xmms2-list ()
  (interactive)
  (switch-to-buffer (xmms2-playlist-buffer))
  (xmms2-call 'xmms2-callback-list "list"))

(defun xmms2-callback-ok (response)
  ())

(defmacro xmms2-simple (command)
  (let ((command-name (intern (concat "xmms2-" command))))
    `(defun ,command-name ()
       (interactive)
       (xmms2-call 'xmms2-callback-ok ,command))))

(xmms2-simple "play")
(xmms2-simple "pause")
(xmms2-simple "stop")
(xmms2-simple "toggle")
(xmms2-simple "next")
(xmms2-simple "prev")

;;; The main playlist

(defface xmms2-song
    '((t :weight bold))
  "Generic face for song"
  :group 'xmms2)

(defface xmms2-artist
    '((t :weight bold))
  "Generic face for song"
  :group 'xmms2)
(defface xmms2-title
    '((t :weight bold))
  "Generic face for song"
  :group 'xmms2)
(defface xmms2-album
    '((t :slant italic))
  "Generic face for song"
  :group 'xmms2)


(defun xmms2-playlist-buffer ()
  (let ((buffer (get-buffer-create "*Playlist*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'xmms2-playlist-mode)
	(xmms2-playlist-mode)))
    buffer))

(define-derived-mode xmms2-playlist-mode special-mode
  "Playlist"
  "Major mode for xmm2 playlist

\\{xmms2-playlist-mode-map}"
  :group 'xmms2)

(progn					;should not be done on reload
  (suppress-keymap xmms2-playlist-mode-map)
  (define-key xmms2-playlist-mode-map " " 'xmms2-toggle)
  (define-key xmms2-playlist-mode-map "n" 'xmms2-next)
  (define-key xmms2-playlist-mode-map "p" 'xmms2-prev)
  (define-key xmms2-playlist-mode-map "s" 'xmms2-stop)
  (define-key xmms2-playlist-mode-map "n" 'xmms2-next)
  (define-key xmms2-playlist-mode-map "g" 'xmms2-list))


(provide 'xmms2)
