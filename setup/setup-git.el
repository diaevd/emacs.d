;;; setup-git.el --- GIT Functions

;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; Author: diabolo <diabolo@veles>
;; Maintainer: diabolo <diabolo@veles>
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
;;   (require 'setup-git)

;;; Code:

(eval-when-compile
  (if (version< emacs-version "27.1")
      (require 'cl)
    (require 'cl-lib)))

;;------------------------------------------------------------------------
;;
;; GIT (magit)
;;
;;------------------------------------------------------------------------
;; ;; (require 'magit)
;; (autoload 'magit-status "magit" nil t)
;; ;; (global-set-key (kbd "M-g r m") 'magit-status)
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (define-key global-map (kbd "C-x g") 'magit-status)
;; ;; (global-set-key (kbd "C-x g s") #'magit-status)
;; (global-set-key (kbd "C-x g d") #'magit-diff)
;; (global-set-key (kbd "C-x g e") #'magit-ediff-popup)
;; (global-set-key (kbd "C-x g b") #'magit-branch)
;; (global-set-key (kbd "C-x g c") #'magit-commit)
;; ;; (require 'git-blamed)
;; (autoload 'git-blamed-mode "git-blamed"
;;   "Minor mode for incremental blame for Git." t)
;; (global-set-key (kbd "C-x g v") 'git-blamed-mode)
(use-package magit ; TODO key bindings and such
  :ensure t
  :bind (("C-x g s" . magit-status)
         ("C-x g d" . magit-diff)
         ("C-x g e" . magit-ediff-popup)
         ("C-x g b" . magit-branch)
         ("C-x g c" . magit-commit))
  )

(provide 'setup-git)
;;; setup-git.el ends here
