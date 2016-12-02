;;; setup-go.el --- Setup GO environment

;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; Author: Evgeny Duzhakov <diabolo@veles>
;; Maintainer: Evgeny Duzhakov <diabolo@veles>
;; Created: 01 Dec 2016
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
;; M-x godef-jump (C-c C-j) - перейти к реализации функции под курсором (вернуться назад, можно через M-*)
;; M-x godef-jump-other-window (C-x 4 C-c C-j) - аналогично "godef-jump" только открывается в новом окне
;; M-x godoc-at-point - покажет документацию по команде под курсором
;; M-x go-goto-imports (C-c C-f i) - перейти к секции "import" текущего файла
;; M-x go-goto-function (C-c C-f f) - перейти к началу функции, внутри которой находится курсор
;; M-x go-goto-arguments (C-c C-f a) - перейти к аргументам текущей функции
;; M-x go-goto-docstring (C-c C-f d) - перейти к комментариям функции
;; M-x go-goto-return-values (C-c C-f r) - перейти к описанию возвращаемого значения для функции
;; M-x beginning-of-defun (C-M-a) - перейти к началу функции
;; M-x end-of-defun (C-M-e) - перейти к концу функции

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'setup-go)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'company)
(require 'flycheck)
(require 'yasnippet)
(require 'multi-compile)
(require 'go-eldoc)
(require 'company-go)

(add-hook 'before-save-hook 'gofmt-before-save)
(setq-default gofmt-command "goimports")
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(setq multi-compile-alist '(
                            (go-mode . (
                                        ("go-build" "go build -v"
                                         (locate-dominating-file buffer-file-name ".git"))
                                        ("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                                         (multi-compile-locate-file-dir ".git"))))
                            ))

(provide 'setup-go)
;;; setup-go.el ends here
