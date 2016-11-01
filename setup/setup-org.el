;;; setup-org.el --- Org Mode

;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; Author: diabolo <diabolo@veles>
;; Maintainer: diabolo <diabolo@veles>
;; Created: 31 Oct 2016
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
;;   (require 'setup-org)

;;; Code:

(eval-when-compile
  (require 'cl))

;;------------------------------------------------------------------------
;;
;; Org-Mode
;;
;;------------------------------------------------------------------------


(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))
;; (setq org-startup-indented t)
;; (setq org-indent-mode t)
;; (setq org-hide-leading-stars t)

(defun my-org ()
  "Create org-mode windows open files"
  (interactive)
  (setq default-frame-alist
        `((width . ,140)
          (height . ,45)
          (top . ,5)
          (left . ,5)
          (user-position . t)
          ))
  (delete-other-windows)
  (split-window-horizontally)
  (setq eik-links-win-w 60)
  (shrink-window-horizontally 32)
  (find-file "~/Documents/org/links.org")
  (dedicated-mode)
  (other-window 1)
  (find-file "~/Documents/org/daily.org")
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 1)
  (find-file "~/Documents/org")
  (other-window 1)
  (find-file "~/Documents/org")
  (other-window 1)
  (other-window 1)
  (dedicated-mode)
  (end-of-buffer)                       ; Go to the end of buffer
  (outline-previous-visible-heading 1)  ; Find the last heading
  (org-cycle)                           ; Make subtree visible
  )

(global-set-key (kbd "<f2> o") 'my-org)

;; (global-set-key (kbd "C-c m") 'org-table-copy-down)
;; (global-set-key (kbd "C-c RET") 'org-insert-heading-after-current)
;(global-set-key (kbd "C-c C-c RET") 'org-insert-heading)

(add-hook 'org-mode-hook (lambda () (interactive) (local-set-key (kbd "C-c m") 'org-table-copy-down)))
(add-hook 'org-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x c") 'org-insert-todo-heading)))
;; (add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-RET") 'org-insert-heading-after-current)))
;; (add-hook 'org-mode-hook (lambda () (interactive) (local-set-key (kbd "<C-return>") 'org-insert-heading)))
;; (add-hook 'org-mode-hook (lambda () (interactive) (local-set-key [(control return)] 'org-insert-heading-after-current)))
;; (add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-\j") 'org-insert-heading-after-current)))
(global-set-key (kbd "C-c c") 'org-capture)
(defun my-notes ()
  "Open org-mode notes files"
  (interactive)
  (split-window-right)
  (other-window 1)
  (find-file "~/Documents/org/notes.org")
  )
(global-set-key (kbd "C-c n") 'my-notes)

(provide 'setup-org)
;;; setup-org.el ends here
