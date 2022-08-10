;;; setup-lisp.el --- List config

;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; Author: Evgeny Duzhakov <diabolo@veles>
;; Maintainer: Evgeny Duzhakov <diabolo@veles>
;; Created: 01 Nov 2016
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
;;   (require 'setup-lisp)

;;; Code:

(eval-when-compile
  (if (version< emacs-version "27.1")
      (require 'cl)
    (require 'cl-lib)))

(straight-use-package 'paredit)

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

;; (show-paren-mode 1)  ;; highlight matching parenthasis
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; nifty documentation at point for lisp files
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package helpful
  :ensure t
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command)

  ;; fix https://github.com/Wilfred/helpful/issues/282
  ;;
  ;; (defun helpful--autoloaded-p (sym buf)
  ;;   "Return non-nil if function SYM is autoloaded."
  ;;   (-when-let (file-name (buffer-file-name buf))
  ;;     (setq file-name (s-chop-suffix ".gz" file-name))
  ;;     (if (boundp 'read-symbol-positions-list)
  ;;         (help-fns--autoloaded-p sym file-name)
  ;;       (help-fns--autoloaded-p sym))))

  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (help-fns--autoloaded-p sym)))

  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines))))

  )

;;;(require 'slime)

(provide 'setup-lisp)
;;; setup-lisp.el ends here
