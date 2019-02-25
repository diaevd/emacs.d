;;; setup-functions.el --- Useful functions

;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; Author: diabolo <diaevd@gmail.com>
;; Maintainer: diabolo <diaevd@gmail.com>
;; Created: 30 Oct 2016
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'setup-functions)

;;; Code:

(eval-when-compile
  (require 'cl))

;;------------------------------------------------------------------------
;;
;; Goto functions
;;
;;------------------------------------------------------------------------
(defun diabolo/goto-column (column)
  "Goto column. The default value of the COLUMN is decrement by -1"
  (interactive "nGoto Column: ")
  (if (> column 0) (setq column (1- column)))
  (move-to-column column t))

(defun diabolo/goto-line-and-column (lc-cond)
  "Allow a specification of LINE:COLUMN or LINE,COLUMN instead of just COLUMN.
Just :COLUMN or ,COLUMN moves to the specified column on the current line.
LINE alone still moves to the beginning of the specified line (like LINE:0 or LINE,0).
The default value of the COLUMN is decrement by -1
because all compilers consider the number of COLUMN from 1 (just for copy-past)"
  (interactive "sLine:Column: ")
  (let (line delim column max-lines)
    (setq max-lines (count-lines (point-min) (point-max)))
    (save-match-data
      (string-match "^\\([0-9]*\\)\\([,:]?\\)\\([0-9]*\\)$" lc-cond)
      (setq line (string-to-number (match-string 1 lc-cond)))
      (setq delim (match-string 2 lc-cond))
      (setq column (string-to-number (match-string 3 lc-cond)))
      (if (not (equal delim "")) (if (> column 0) (setq column (1- column))))
      (if (= 0 line) (setq line (line-number-at-pos)))
      (if (> line max-lines) (setq line max-lines))
      ;; (goto-line line)
      (push-mark)
      (goto-char (point-min))
      (if (eq selective-display t)
          (re-search-forward "[\n\C-m]" nil 'end (1- line))
        (forward-line (1- line)))
      (move-to-column column)
      (message "Marker set to line %d column %s" (line-number-at-pos) (current-column))
      )))

(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to shutdown save buffers and
shutdown the emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers or active clients
or frame.  If so an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ;; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames (or (> (length server-clients) 1)
                                       (> (length (frame-list)) 1)
                                       ))

    ;; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
        (x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ;; Save the current frame.
    (setq new-frame (selected-frame))

    ;; When displaying the number of clients and frames:
    ;; subtract 1 from the clients for this client.
    ;; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when (or (not active-clients-or-frames)
              (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

      ;; If the user quits during the save dialog then don't exit emacs.
      ;; Still close the terminal though.
      (let((inhibit-quit t))
        ;; Save buffers
        (with-local-quit
          (save-some-buffers))

        (if quit-flag
            (setq quit-flag nil)
          ;; Kill all remaining clients
          (progn
            (dolist (client server-clients)
              (server-delete-client client))
            ;; Exit emacs
            (kill-emacs)))
        ))

    ;; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )


(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-base-buffer buffer))
                 (or
                  (buffer-file-name buffer)
                  (progn
                    (set-buffer buffer)
                    (and buffer-offer-save (> (buffer-size) 0))))
                 )
        (setq modified-found t)
        )
      )
    modified-found
    )
  )

;;

(defun stm-create-initial-frame ()
  "Place initial frame on screen and visit the *scratch* buffer in it.
This function can be called from emacsclient to move the created
frame to a convenient place. The frame will open with the
*scratch* buffer. Example:
emacsclient -c -n -e '(stm-create-initial-frame)'
With this function you can start your Emacs as daemon and leave
it running. Before you log out you just close all frames. The
next time you need Emacs you just connect to the running daemon
and use this function to open a new frame. Use
`initial-frame-alist' to define the position and size of the
frame."
  (interactive)
  (when window-system
    (let ((xpos (or (cdr (assq 'left initial-frame-alist)) 0))
          (ypos (or (cdr (assq 'top initial-frame-alist)) 0))
          (rows (or (cdr (assq 'height initial-frame-alist)) 210))
          (cols (or (cdr (assq 'width initial-frame-alist)) 54)))
      (set-frame-position (selected-frame) xpos ypos)
      (set-frame-size (selected-frame) cols rows)))
  (switch-to-buffer "*scratch*" t)
  (lisp-interaction-mode))

;;; moved to init.el
;; (defun 2-windows-vertical-to-horizontal ()
;;   (let ((buffers (mapcar 'window-buffer (window-list))))
;;     (when (= 2 (length buffers))
;;       (delete-other-windows)
;;       (set-window-buffer (split-window-horizontally) (cadr buffers)))))
;; (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

(defun diabolo/transpose-buffers (&optional arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun diabolo/reformat-region (&optional b e)
  (interactive "r")
  (when (not (buffer-file-name))
    (error "A buffer must be associated with a file in order to use REFORMAT-REGION."))
  (when (not (executable-find "clang-format"))
    (error "clang-format not found."))
    ;; (goto-char (point-min))
    ;; (push-mark)
    (shell-command-on-region b e
                             "clang-format"
                             (current-buffer) t)
    (indent-region b e))

(defun diabolo/reformat-buffer ()
  (interactive)
  (setq cur (point))
  (diabolo/reformat-region (point-min) (point-max))
  (goto-char cur)
  )

(provide 'setup-functions)
;;; setup-functions.el ends here
