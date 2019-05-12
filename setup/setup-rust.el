;;; setup-rust.el --- Setup Rust environment

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

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'setup-rust)
;;; Code:

(eval-when-compile
  (require 'cl))

;;; Simple setup with racer, cargo and flycheck
;; (require 'rust-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)
;; ;; For automatic completions, customize company-idle-delay and company-minimum-prefix-length
;;
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
;; (add-hook 'rust-mode-hook #'flycheck-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; Setup with rls, cargo, flyckeck

(defvar diabolo/use-rustic nil)
(defvar diabolo/use-rustic-elgot nil)
(defvar diabolo/use-ra-lsp nil)
;; (setenv "RUST_BACKTRACE" "full")
;; (setenv "RUST_LOG" "rls::=debug")
;; (setenv "RUST_WRAPPER" "sccache")

;; (require 'ra-emacs-lsp)

;; lsp-rust: Rust support for lsp-mode using the Rust Language Server.
;; https://github.com/emacs-lsp/lsp-rust
;; lsp-rust is deprecated around 20190105
(unless diabolo/use-rustic
  (when (< (car (pkg-info-package-version 'lsp-mode)) 20190105)
    (use-package lsp-rust
      :ensure t
      :after lsp-mode
      )))

;; rust-mode: major-mode for editing rust files
;; https://github.com/rust-lang/rust-mode
(unless diabolo/use-rustic
  (use-package rust-mode
    :ensure t
    :hook ((rust-mode . (lambda ()
                          (when (< (car (pkg-info-package-version 'lsp-mode)) 20190105)
                            (lsp-rust-set-goto-def-racer-fallback t) ;; Now is set by default
                            (lsp-ui-doc-enable-eldoc)
                            (flycheck-rust-setup)
                            (flycheck-mode)
                            (lsp-ui-mode)
                            (company-mode))
                          (lsp-rust-enable)
                          ;; (lsp-ui-sideline-mode)
                          ;; (lsp-ui-doc-mode)
                          ;;
                          ;; (eldoc-mode)
                          ;; (racer-mode)
                          ;; (smart-dash-mode)
                          ))
           ;; format rust buffers using rustfmt(if it is installed)
           (rust-mode . (lambda ()
                          (add-hook 'before-save-hook
                                    (lambda ()
                                    (time-stamp)
                                    (lsp-format-buffer)) nil t)))
           (rust-mode . (lambda ()
                          (when (< (car (pkg-info-package-version 'lsp-mode)) 20190105)
                            (set (make-local-variable 'company-backends)
                                 '((company-lsp company-files :with company-yasnippet)
                                   (company-dabbrev-code company-dabbrev)))))))
    :bind (:map rust-mode-map
                ("C-c C-r C-v" . wh/rust-toggle-visibility)
                ("C-c C-r C-m" . wh/rust-toggle-mutability)
                ("C-c C-r C-s" . wh/rust-vec-as-slice)
                ([?\t] . #'company-indent-or-complete-common))
    :ensure-system-package
    ((rustfmt . "rustup component add rustfmt-preview")
     (racer . "cargo install racer")
     (rls . "rustup component add rls-preview rust-analysis rust-src"))
    :config
    (setq rust-indent-method-chain t)
    ;; For automatic completions, customize company-idle-delay and company-minimum-prefix-length
    (setq company-tooltip-align-annotations t)

    (defun wh/rust-toggle-mutability ()
      "Toggle the mutability of the variable at point."
      (interactive)
      (save-excursion
        (racer-find-definition)
        (back-to-indentation)
        (forward-char 4)
        (if (looking-at "mut ")
            (delete-char 4)
          (insert "mut "))))

    (defun wh/rust-toggle-visibility ()
      "Toggle the public visibility of the function at point."
      (interactive)
      (save-excursion
        ;; If we're already at the beginning of the function definition,
        ;; `beginning-of-defun' moves to the previous function, so move elsewhere.
        (end-of-line)

        (beginning-of-defun)
        (if (looking-at "pub ")
            (delete-char 4)
          (insert "pub "))))

    (defun wh/rust-vec-as-slice ()
      "Convert the vector expression at point to a slice.
foo -> &foo[..]"
      (interactive)
      (insert "&")
      (forward-symbol 1)
      (insert "[..]"))))

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; cargo-mode: execute cargo commands easily
;; https://github.com/kwrooijen/cargo.el
(unless diabolo/use-rustic
  (use-package cargo
    :ensure t
    :after rust-mode
    :hook ((rust-mode . cargo-minor-mode))))

(unless diabolo/use-rustic
  (use-package toml-mode
    :ensure t))

(when diabolo/use-rustic
  (when diabolo/use-rustic-elgot
    (use-package eglot
      :ensure t
      :config
      (setq eglot-auto-display-help-buffer t)))

  (use-package rustic
    :ensure t
    ;; :after eglot
    :commands rustic-mode
    :mode ("\\.rs\\'" . rustic-mode)
    :hook (rust-mode . rustic-mode)
    :config
    (when diabolo/use-rustic-elgot (setq rustic-rls-pkg 'eglot))))

(provide 'setup-rust)
;;; setup-rust.el ends here
