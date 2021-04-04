;;; setup-rust.el --- Setup Rust environment

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.
;;
;; Author: Evgeny Duzhakov <diabolo@veles>
;; Maintainer: Evgeny Duzhakov <diabolo@veles>
;; Created: 01 Dec 2016
;; Updated: 07 Apr 2020
;; Version: 0.02
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

(use-package flycheck
  :bind (:map flycheck-mode-map
              ("C-z C-l" . #'flycheck-list-errors)
              ("M-<up>" . #'flycheck-previous-error)
              ("M-<down>" . #'flycheck-next-error)))

(use-package treemacs)

(use-package treemacs-projectile
  :ensure t
  :after treemacs)

(use-package lsp-treemacs
  :ensure t
  :after treemacs lsp)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :bind
  (:map lsp-mode-map
        ("C-c C-a" . lsp-execute-code-action)
        ("C-c C-j" . lsp-rust-analyzer-join-lines)
        ("C-c C-i" . lsp-rust-analyzer-inlay-hints-mode))
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-eldoc-hook nil)
  (eldoc-mode t))

(use-package diminish
  :defer 5
  :config
  (diminish 'org-indent-mode)
  (diminish 'projectile-mode)
  (diminish 'undo-tree-mode))

(use-package cargo
  :ensure t
  :after rust-mode
  :hook ((rust-mode . cargo-minor-mode)))

(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook
  ((rust-mode . (lambda ()
                  (lsp)
                  (eldoc-mode t)))
   (lsp-after-initialize . (lambda ()
                             (lsp-rust-analyzer-inlay-hints-mode t)
                             (eldoc-mode t)
                             ;; (global-eldoc-mode -1)
                             ;; You can alternatively (setq-local eldoc-documentation-function #'ignore)
                             ;; in buffers for which you wish eldoc to have no effect,
                             ;; without disabling it elsewhere.
                             ))
   ;; (rust-before-save . #'lsp-format-buffer)
   )
  :bind (:map rust-mode-map
              ([?\t] . #'company-indent-or-complete-common)
              ("C-c C-e" . #'lsp-rust-analyzer-expand-macro))
  :config
  (setq rust-format-on-save t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-macro-expansion-method 'dia/rust-analyzer-macro-expand
        ;; lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand
        ;; lsp-rust-analyzer-macro-expansion-method 'lsp-rust-analyzer-macro-expansion-default
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-full-docs t
        lsp-rust-server 'rust-analyzer)
  (defvar dia/rust-rustfmt-bin-v (replace-regexp-in-string "\r?\n\\'" "" (shell-command-to-string "rustup +stable which rustfmt")))
  (setq rust-rustfmt-bin dia/rust-rustfmt-bin-v)
  (setq rustic-rustfmt-bin dia/rust-rustfmt-bin-v)

  (defun dia/rust-analyzer-macro-expand (result)
    "Default method for displaying macro expansion results."
    (interactive)
    (let* ((root (lsp-workspace-root default-directory))
           (buf (get-buffer-create
                 (format "*rust-analyzer macro expansion %s*" root))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; wrap expanded macro in a main function so we can run rustfmt
          (insert "fn __main(){")
          ;; rustfmt complains about $s
          (insert (replace-regexp-in-string "\\$" "" result))
          (insert "}")
          ;; /wrap expanded macro
          (rust-mode)
          (ignore-errors (rust--format-call buf))
          ;; (lsp-mode)
          (with-current-buffer buf
            (save-excursion
              ;; remove fn __main() {
              (goto-char (point-min))
              (delete-region (point-min) (line-end-position))
              (delete-blank-lines)
              (goto-char (point-max))
              ;; remove } from fn __main()
              (forward-line -1)
              (delete-region (line-beginning-position) (point-max))
              ;; reindent buffer to left
              (indent-buffer)
              (goto-char (point-max))
              ;; clean blanked line
              (delete-blank-lines))))
        (read-only-mode t)
        (local-set-key "q" 'kill-current-buffer))
      (display-buffer buf)))

  (eldoc-mode t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :bind
  (:map lsp-ui-mode-map
        ;; ("C-c C-a" . lsp-ui-sideline-apply-code-actions)
        ("C-c v" . lsp-ui-imenu)
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-d" . lsp-ui-peek-find-definitions)
        ("C-c C-h" . dia/lsp-ui-doc-toggle))

  :config
  (setq lsp-ui-doc-alignment 'window
        lsp-ui-doc-position 'top
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-flycheck t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-code-actions-prefix "💡 "
        lsp-ui-sideline-update-mode 'point
        lsp-eldoc-enable-hover t)
  (eldoc-mode t)

  (defvar dia/lsp-ui-doc-toggle-v nil)
  (defun dia/lsp-ui-doc-toggle ()
    "Toggle the mutability of the variable at point."
    (interactive)
    (if (and
         (lsp-ui-doc--get-frame)
         (frame-visible-p (lsp-ui-doc--get-frame))) ;; dia/lsp-ui-doc-toggle-v
        (progn
          (lsp-ui-doc-hide)
          (setq dia/lsp-ui-doc-toggle-v nil))
      (lsp-ui-doc-show)
      (setq dia/lsp-ui-doc-toggle-v t)))
)

;;; DAP (Debug Adapter Protocol Mode)
;;; https://github.com/emacs-lsp/dap-mode#configuration
(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :bind
    (:map dap-mode-map
          (("<f12>" . dap-debug)
           ("<f8>" . dap-continue)
           ("<f9>" . dap-next)
           ("<M-f11>" . dap-step-in)
           ("C-M-<f11>" . dap-step-out)
           ("<f7>" . dap-breakpoint-toggle)))
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

;;; Company LSP
;;; https://github.com/tigersoldier/company-lsp
(use-package company-lsp
  :defer t
  ;; :custom
  ;; (company-lsp-cache-candidates 'auto)
  :config
  (setq company-show-numbers t)
  (setq company-lsp-cache-candidates 'auto))

;;; Company TabNine
;;; https://github.com/TommyX12/company-tabnine
;;; Prerequisite: Execute M-x company-tabnine-install-binary to install the TabNine binary for your system.
;;; Commentary: This is enabled by default, if ever you find it not good enough for a particular completion,
;;; simply use M-q to immediately switch to default backends.
;; (use-package company-tabnine
;;   :defer t
;;   ;; :custom
;;   ;; (company-tabnine-max-num-results 9)
;;   :bind
;;   (("M-q" . company-other-backend)
;;    ("C-z t" . company-tabnine))
;;   :hook
;;   (lsp-after-open . (lambda ()
;;                       (setq company-tabnine-max-num-results 3)
;;                       (add-to-list 'company-transformers 'company//sort-by-tabnine t)
;;                       (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
;;   (kill-emacs . company-tabnine-kill-process)
;;   :config
;;   ;; Enable TabNine on default
;;   (add-to-list 'company-backends #'company-tabnine)

;;   ;; Integrate company-tabnine with lsp-mode
;;   (defun company//sort-by-tabnine (candidates)
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-lsp
;;             candidates-tabnine)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-tabnine))
;;             (push candidate candidates-lsp)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-lsp (nreverse candidates-lsp))
;;         (setq candidates-tabnine (nreverse candidates-tabnine))
;;         (nconc (seq-take candidates-tabnine 3)
;;                (seq-take candidates-lsp 6))))))

;;; Setup with rls, cargo, flyckeck

(provide 'setup-rust)
;;; setup-rust.el ends here
