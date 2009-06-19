;; (C) 2009 Rémi Vanicat <vanicat@debian.org>

;; lagn.el is a music player for Emacs that uses xmms2 as its backend
;; lagn stand for Lacking A Good Name

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

;; You can use lagn-list to have the current playlist.
;;
;; It create a *Playlist* buffer, with the following key
;;  SPC		lagn-toggle  (that is play/pause)
;;  g		lagn-list
;;  n		lagn-next
;;  p		lagn-prev
;;  s		lagn-stop

;; you can also directly use M-x lagn-play to start playback, and
;; M-x lagn-status to see the current playing situation

;; note that there is no automatic update on the Playlist for now

;; note also that I won't recommand it for manipulating big playlist

;;; Installing:

;; put this file in some direcories in your load path
;; add (require 'lagn) to you .emacs

;; you can also add keybinding: for example, for my multimedia keys:

;; (global-set-key [<XF86AudioPlay>] 'lagn-toggle)
;; (global-set-key [<XF86Back>] 'lagn-prev)
;; (global-set-key [<XF86Forward>] 'lagn-next)

;;; TODO
;; searching
;; edit current playlist
;; a mode to view/edit other playlist
;; a mode to view/edit collection
;; a mode to view current status
;; maybe status bar integration


(defvar lagn-playlist nil
  "The lagn playlist,
Its format is
  (POS LIST)
where POS is the current position and LIST is a list of
  (ID ARTIST ALBUM TITLE URL)
Where ID is the xmms2 id of the song, and ARTIST ALBUM TITLE maybe be nil
if xmms2 doesn't know them")

(defvar lagn-now-playing nil
  "The song that is being played now")

(defvar lagn-status nil
  "The status of xmms2, a symbol
can be paused, stopped, playing or nil
nil mean that there is noconnection or there was an error")

(defvar lagn-info-cache (make-hash-table :weakness 'value))

(defgroup lagn ()
  "lagn is a client for xmms2 written in emacs lisp")

(defcustom lagn-command
  "nyxmms2"
  "The command to run for xmms2 command. Must be API compatible with nyxmms2"
  :group 'lagn
  :type 'string)

(defvar lagn-process ())
(defvar lagn-callback-queue ()
  "queue of callback to be run from the filter")


;;; some function to read anwser from nyxmms2

(defun lagn-decode-info (info)
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
      (puthash id result lagn-info-cache)
      result)))


(defun lagn-decode-list (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (current-pos id result list)
      (while (search-forward-regexp "^\\(->\\|  \\)\\[\\([0-9]+\\)/\\([0-9]+\\)\\] \\(.*\\)$" () t)
	(when (string= (match-string 1) "->")
	  (setq current-pos (string-to-number (match-string 2))))
	(setq id (string-to-number (match-string 3)))
	(setq result (gethash id lagn-info-cache))
	(unless result
	  (setq result (cons id (match-string 4)))
	  (lagn-info id))
	(push result list))
      (cons current-pos (nreverse list)))))


;;; function for the connection itself

(defun lagn-clean ()
  (when (processp lagn-process)
    (with-current-buffer (process-buffer lagn-process)
      (delete-region (point-min) (point-max))
      (setq lagn-callback-queue ())
      (setq lagn-process ()))))


(defun lagn-process-sentinel (proc event)
  ;; Is this only called when the process died ?
  (lagn-clean))


(defun lagn-process-filter (proc str)
  (with-current-buffer (process-buffer lagn-process)
    (goto-char (process-mark lagn-process))
    (insert str)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (while (search-forward-regexp "xmms2> *$" () t)
      (let ((response (buffer-substring (point-min) (match-beginning 0))))
	(delete-region (point-min) (match-end 0))
	(apply (pop lagn-callback-queue) (list response))))))


(defun lagn-callback-message (response)
  (message response))


(defun lagn-init-process ()		;TODO: add option for server and such
  (setq lagn-process (start-process "nyxmms2" " *nyxmms2*" lagn-command))
  (setq lagn-callback-queue (append lagn-callback-queue (list 'lagn-callback-message)))
  (set-process-filter lagn-process 'lagn-process-filter)
  (set-process-sentinel lagn-process 'lagn-process-sentinel)
  (lagn-status))


(defun lagn-ensure-connected ()
  (unless (and lagn-process
	       (eq (process-status lagn-process)
		   'run))
    (lagn-init-process)))


(defun lagn-call (callback command &rest args)
  (lagn-ensure-connected)
  (let ((string (apply 'format command args)))
    (process-send-string lagn-process string)
    (process-send-string lagn-process "\n")
    (setq lagn-callback-queue (append lagn-callback-queue (list callback)))))


;;; the commands

(defun lagn-exit-process ()
  (lagn-call 'ignore "exit"))


(defun lagn-callback-current-info (response)
  (setq lagn-now-playing (lagn-decode-info response)))


(defun lagn-callback-status (response)
  (unless (string-match "^\\(Paused\\|Stopped\\|Playing\\):" response)
    (setq lagn-status ())
    (error "wrong status message"))
  (cond
    ((string= (match-string 1 response) "Playing")
     (setq lagn-status 'playing))
    ((string= (match-string 1 response) "Paused")
     (setq lagn-status 'paused))
    ((string= (match-string 1 response) "Stopped")
     (setq lagn-status 'stopped)))
  (message response))


(defun lagn-status ()
  (interactive)
  (lagn-call 'lagn-callback-status "status")
  (lagn-call 'lagn-callback-current-info "info"))


(defun lagn-song-string (song)
  (if (listp (cdr song))
      (destructuring-bind (it artist album title url) song
	(unless artist (setq artist "Unknown"))
	(unless album (setq album "Unknown"))
	(unless title (setq title url))
	(setq artist (propertize artist 'face 'lagn-artist))
	(setq album (propertize album 'face 'lagn-artist))
	(setq title (propertize title 'face 'lagn-artist))
	(format "%s\n\tby %s from %s" title artist album))
      (propertize (cdr song) 'face 'lagn-song)))


(defun lagn-playlist-insert-song (string song num)
  (insert (propertize string
		      'lagn-num num
		      'lagn-id (car song)
		      'lagn-song song)))


(defun lagn-callback-list (response)
  (setq lagn-playlist (lagn-decode-list response))
  (with-current-buffer (lagn-playlist-buffer)
    (let ((buffer-read-only ())
	  (current-point (point))
	  (num 1)
	  (song-string)
	  (current-marker)
	  (current (car lagn-playlist)))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (cond ((eq lagn-status 'playing) "Playing")
		    ((eq lagn-status 'paused) "Paused")
		    ((eq lagn-status 'stopped) "Stopped")
		    ('t "Unkown")))
      (insert "\n\n")

      (dolist (song (cdr lagn-playlist))
	(when (= num current)
	  (setq overlay-arrow-position (point-marker)))
	(setq song-string (concat " " (lagn-song-string song) "\n"))
	(lagn-playlist-insert-song song-string song num)
	(setq num (1+ num)))
      (goto-char current-point))))


(defun lagn-list ()
  (interactive)
  (lagn-status)
  (switch-to-buffer (lagn-playlist-buffer))
  (lagn-call 'lagn-callback-list "list"))


(defun lagn-callback-ok (response)
  ())


(defmacro lagn-simple (command)
  (let ((command-name (intern (concat "lagn-" command))))
    `(defun ,command-name ()
       (interactive)
       (lagn-call 'lagn-callback-ok ,command))))

(lagn-simple "play")
(lagn-simple "pause")
(lagn-simple "stop")
(lagn-simple "toggle")
(lagn-simple "next")
(lagn-simple "prev")


;; TODO: add docstrings
(defmacro lagn-command-with-patern (command &optional pos xmms-command)
  (let ((command-name (intern (concat "lagn-" command)))
	(xmms-command (or xmms-command command)))
    `(defun ,command-name (pattern)
       (interactive ,(if pos "sPattern Or Position: " "sPattern: "))
       (lagn-call 'lagn-callback-ok ,(concat xmms-command " %s") pattern))))

(lagn-command-with-patern "jump" t)
(lagn-command-with-patern "remove" t)
(lagn-command-with-patern "add")
(lagn-command-with-patern "insert" () "add -n")


(defun lagn-callback-info (result)
  (let ((song (lagn-decode-info result)))
    (with-current-buffer (lagn-playlist-buffer)
      (save-excursion
	(let* ((beg (text-property-any (point-min) (point-max) 'lagn-id (car song)))
	       (num (get-text-property beg 'lagn-num))
	       (buffer-read-only ()))
	  (when beg
	    (goto-char (+ 3 beg))
	    (lagn-playlist-insert-song (lagn-song-string song) song num)
	    (delete-region (point)
			   (1- (next-single-property-change beg 'lagn-id () (point-max))))))))))


(defun lagn-info (id)
  (lagn-call 'lagn-callback-info "info id:%d" id))


;;; The main playlist

(defface lagn-song
    '((t :weight bold))
  "Generic face for song"
  :group 'lagn)

(defface lagn-artist
    '((t :weight bold))
  "Generic face for song"
  :group 'lagn)
(defface lagn-title
    '((t :weight bold))
  "Generic face for song"
  :group 'lagn)
(defface lagn-album
    '((t :slant italic))
  "Generic face for song"
  :group 'lagn)


(defun lagn-playlist-buffer ()
  (let ((buffer (get-buffer-create "*Playlist*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'lagn-playlist-mode)
	(lagn-playlist-mode)))
    buffer))




(define-derived-mode lagn-playlist-mode ()
  "Playlist"
  "Major mode for lagn playlist

\\{lagn-playlist-mode-map}"
  :group 'lagn
  (setq buffer-undo-list t)
  (setq truncate-lines t))

(put 'lagn-playlist-mode 'mode-class 'special)


(progn					;should not be done on reload
  (suppress-keymap lagn-playlist-mode-map)
  (define-key lagn-playlist-mode-map " " 'lagn-toggle)
  (define-key lagn-playlist-mode-map "n" 'lagn-next)
  (define-key lagn-playlist-mode-map "p" 'lagn-prev)
  (define-key lagn-playlist-mode-map "s" 'lagn-stop)
  (define-key lagn-playlist-mode-map "n" 'lagn-next)
  (define-key lagn-playlist-mode-map "g" 'lagn-list))


(provide 'lagn)
