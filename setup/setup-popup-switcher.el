;;; setup-popup-switcher.el --- PopUP Switcher

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
;;   (require 'setup-popup-switcher)

;;; Code:

(eval-when-compile
  (if (version< emacs-version "27.1")
      (require 'cl)
    (require 'cl-lib)))

;;------------------------------------------------------------------------
;;
;; popup-switcher
;;
;;------------------------------------------------------------------------
(use-package popup-switcher
  :requires (eassist)
  :config

  (defun my-c-mode-common-hook ()
    (define-key c-mode-base-map (kbd "M-h") 'eassist-switch-h-cpp)
    (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  (add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

  ;; (setq psw-in-window-center t)
  (global-set-key [f7] 'psw-switch-buffer)
  ;; (global-set-key [f8] 'psw-switch-projectile-files)

  (eval-after-load "eassist"
    '(global-set-key [f8] 'psw-switch-function))

  (defun psw-list-methods ()
    (interactive)
    (psw-switcher
     :items-list (eassist-mode)
     :item-name-getter 'car
     :switcher (psw-compose 'goto-char 'cdr)))

  ;; Redefine switch file/buffers to
  ;; (global-set-key (kbd "C-x C-f") 'psw-navigate-files)
  ;; (global-set-key (kbd "C-x C-b") 'psw-switch-buffer)
  (global-set-key (kbd "C-<f8>") 'psw-list-methods)

;; (eval-after-load "eassist"
;;    '(global-set-key (kbd "M-o") 'psw-switch-h-cpp))

;; (eval-after-load "eassist"
;;   '(global-set-key (kbd "M-m") 'psw-list-methods))

;;      (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
;;      (define-key c-mode-base-map (kbd "M-m") 'eassist-list-method))
  )

;; (add-hook 'go-mode-hook 'psw-list-methods)

;; (global-set-key [f8] 'psw-switch-function)

;; (defun psw-switch-h-cpp ()
;;    (interactive)
;;    (psw-switcher
;;     :items-list (eassist-switch-h-cpp)
;;     :item-name-getter 'car
;;     :switcher (psw-compose 'goto-char 'cdr)))


(provide 'setup-popup-switcher)
;;; setup-popup-switcher.el ends here
