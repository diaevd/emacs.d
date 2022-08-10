;;; setup-rust.el --- Setup Rust environment -*- no-byte-compile: t -*-

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
  (if (version< emacs-version "27.1")
      (require 'cl)
      (require 'cl-lib)))

;; (require 'use-package)

(use-package diminish
    ;; :defer 5
    :config
    (diminish 'org-indent-mode)
    (diminish 'projectile-mode)
    (diminish 'undo-tree-mode))

(use-package flycheck
    :bind (:map flycheck-mode-map
                ("C-z C-l" . #'flycheck-list-errors)
                ("M-<up>" . #'flycheck-previous-error)
                ("M-<down>" . #'flycheck-next-error)))

(use-package treemacs
    :ensure t)

(use-package treemacs-projectile
    :ensure t
    :after treemacs)

(use-package lsp-treemacs
    :ensure t
    :after treemacs lsp-mode)

(use-package lsp-mode
  :init
  :hook ((lsp-after-initialize . dia/lsp-after-init-hook)
         (lsp-after-open . dia/lsp-after-open-hook))

  :custom
  (lsp-keymap-prefix "C-c C-l")
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  :bind
  (:map lsp-mode-map
        ;; ("C-c C-a" . lsp-execute-code-action)
        ("C-c C-a" . helm-lsp-code-actions)
        ("C-c C-j" . lsp-rust-analyzer-join-lines)
        ("C-c C-i" . lsp-rust-analyzer-inlay-hints-mode))
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-log-io t)
  ;; (setq lsp-log-io-allowlist-methods t)
  (setq lsp-signature-auto-activate nil)
  (lsp-semantic-tokens-mode 1)
  ;; (push 'dia/lsp-after-init-hook lsp-after-initialize-hook)
  ;; (setq lsp-eldoc-hook nil)
  ;; (eldoc-mode t)
  )

(defun dia/lsp-after-init-hook ()
  (message "LSP AFTER INIT HOOK")
  (dia/lsp-hook))

(defun dia/lsp-after-open-hook ()
  (message "LSP AFTER OPEN HOOK")
  (dia/lsp-hook))

(defun dia/lsp-ui-mode-hook ()
  (message "LSP UI MODE HOOK: %s" company-backends)
  ;; (dia/lsp-hook)
  )

(defun dia/lsp-hook ()
  ;; (set (make-local-variable 'company-backends)
  ;;      '((company-capf           ; what is this? it is capture backend
  ;;                                ; company-lsp - fucking shit
  ;;         company-files          ; files & directory
  ;;         company-keywords       ; keywords
  ;;         company-yasnippet)
  ;;        (company-abbrev company-dabbrev)))
  )

(use-package toml-mode
    :ensure t)

(use-package lsp-ui
    :ensure t
    ;; :after lsp-mode
    ;; :hook lsp-mode
    :hook ((lsp-ui-mode . dia/lsp-ui-mode-hook))
    :bind
    (:map lsp-ui-mode-map
          ;; ("C-c C-a" . lsp-ui-sideline-apply-code-actions)
          ("C-c v" . lsp-ui-imenu)
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-d" . lsp-ui-peek-find-definitions)
          ("C-c i" . lsp-ui-peek-find-implementation)
          ("C-c C-h" . dia/lsp-ui-doc-toggle)
          ("C-M-i" . dia/lsp-ui-doc-toggle))

    :config
    (setq lsp-ui-doc-alignment 'window
          lsp-ui-doc-position 'top
          ;; ???
          lsp-ui-peek-always-show t
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-symbol t
          lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-show-code-actions t
          lsp-ui-sideline-show-flycheck t
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-code-actions-prefix "💡 "
          lsp-ui-sideline-update-mode 'point
          lsp-eldoc-enable-hover t)
    )

(defvar dia/lsp-ui-doc-toggle-v nil)
(defun dia/lsp-ui-doc-toggle ()
  "Toggle the doc on symbol at point."
  (interactive)
  (if (and
       (lsp-ui-doc--get-frame)
       (frame-visible-p (lsp-ui-doc--get-frame))) ;; dia/lsp-ui-doc-toggle-v
      (progn
        (lsp-ui-doc-hide)
        (setq dia/lsp-ui-doc-toggle-v nil))
      (lsp-ui-doc-show)
      (setq dia/lsp-ui-doc-toggle-v t)))

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

    ;; (require 'dap-lldb)
    (require 'dap-hydra)
    (require 'dap-gdb-lldb)
    (require 'dap-cpptools)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)

    ;; (dap-register-debug-template
    ;;  "Rust::LLDB Run Configuration"
    ;;  (list :type "lldb"
    ;;        :request "launch"
    ;;        :name "LLDB::Run"
    ;;        :gdbpath "rust-lldb"
    ;;        ;; uncomment if lldb-mi is not in PATH
    ;;        ;; :lldbmipath "path/to/lldb-mi"
    ;;        ))

    (dap-register-debug-template
     "Rust::GDB Run Configuration"
     (list :type "gdb"
           :request "launch"
           :name "GDB::Run"
	   :gdbpath "rust-gdb"
           :gdbpath "rust-gdb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           :target nil
           :cwd nil
           ))))

;;; Company LSP
;;; https://github.com/tigersoldier/company-lsp
(use-package company-lsp
  :defer t
  ;; :custom
  ;; (company-lsp-cache-candidates 'auto)
  :config
  ;; (setq company-show-numbers t) - deprecated after 0.9.14 (now use show-quick-access)
  ;; (setq company-show-quick-access t)
  (setq company-lsp-cache-candidates 'auto)
  ;; (push 'company-lsp company-backends)
  )

;;; Company TabNine
;;; https://github.com/TommyX12/company-tabnine
;;; Prerequisite: Execute M-x company-tabnine-install-binary to install the TabNine binary for your system.
;;; Commentary: This is enabled by default, if ever you find it not good enough for a particular completion,
;;; simply use M-q to immediately switch to default backends.
(use-package company-tabnine
    ;; :defer t
    :ensure t
    :disabled t
    ;; :custom
    ;; (company-tabnine-max-num-results 9)
    :bind
    (("M-q" . company-other-backend)
     ("C-z t" . company-tabnine))
    :hook
    (lsp-after-open . (lambda ()
                        (setq company-tabnine-max-num-results 4)
                        (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                        ;; (add-to-list 'company-backends 'company-tabnine)))
                        (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
    ;; (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
    (kill-emacs . company-tabnine-kill-process)
    :config
    ;; Enable TabNine on default
    ;; (add-to-list 'company-backends #'company-tabnine)
    (setq company-tabnine-max-num-results 3)
    (setq company-show-quick-access t)

    ;; Integrate company-tabnine with lsp-mode
    (defun company//sort-by-tabnine (candidates)
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
          candidates
          (let ((candidates-table (make-hash-table :test #'equal))
                candidates-lsp
                candidates-tabnine)
            (dolist (candidate candidates)
              (if (eq (get-text-property 0 'company-backend candidate)
                      'company-tabnine)
                  (unless (gethash candidate candidates-table)
                    (push candidate candidates-tabnine))
                  (push candidate candidates-lsp)
                  (puthash candidate t candidates-table)))
            (setq candidates-lsp (nreverse candidates-lsp))
            (setq candidates-tabnine (nreverse candidates-tabnine))
            (nconc (seq-take candidates-tabnine 3)
                   (seq-take candidates-lsp 20))))))


(defun dia/rust-analyzer-macro-expand (result)
  "Default method for displaying macro expansion results."
  (interactive)
  (let* ((root (lsp-workspace-root default-directory))
         (buf (get-buffer-create
               (format "*rust-analyzer macro expansion %s*" root))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (fmt-done "*rustfmt*"))
        (erase-buffer)
        ;; wrap expanded macro in a main function so we can run rustfmt
        (insert "fn __main(){\n")
        ;; rustfmt complains about $s
        (insert (replace-regexp-in-string "\\$" "" result))
        (insert "}\n")
        ;; /wrap expanded macro
        ;; (rusti-mode)
        ;; (ignore-errors (rust--format-call buf))
        ;; (rustic-macro-expansion-mode)
        (rustic-mode)
        ;; (ignore-errors
        (lexical-let ((buf buf)
                      (fmt-done fmt-done))
          (set-process-sentinel
           (rustic-format-buffer)
           (lambda (p e)
             (with-current-buffer buf
               (read-only-mode -1)
               (replace-buffer-contents fmt-done)
               (kill-buffer fmt-done)
               ;; remove fn __main() {
               (goto-char (point-min))
               (delete-region (point-min) (line-end-position))
               (goto-char (point-min))
               (delete-blank-lines)
               ;; remove } at end of fn __main()
               (goto-char (point-max))
               (forward-line -1)
               (delete-region (line-beginning-position) (point-max))
               (indent-region (point-min) (point-max))
               ;; clean blanked line
               (goto-char (point-max))
               (delete-blank-lines)
               (delete-trailing-whitespace (point-min) (point-max))
               ;;
               (local-set-key "q" 'kill-current-buffer)
               (goto-char (point-min))
               ;; (read-only-mode 1)
               (rustic-macro-expansion-mode)
               (display-buffer buf)))))))))

;;; prepare variable for set rust-rustfmt-bin or rustic-rustfmt-bin
(defvar dia/rust-rustfmt-bin-v
  (replace-regexp-in-string
   "\r?\n\\'" ""
   (shell-command-to-string "rustup +stable which rustfmt")))

;;;;;;; Use rust-mode
;; (eval-when-compile
;;   (require 'setup-rust-mode))
;; (use-package cargo
;;   ;; :requires hydra
;;   :ensure t
;;   ;; :after rust-mode
;;   ;; :hook ((rust-mode . #'cargo-minor-mode))
;;   ;; :config
;;   )

;; (defun dia/toggle-rust-backtrace ()
;;   "Toggle between Rust backtrace enabled and disabled."
;;   (interactive)
;;   (if (not cargo-process--enable-rust-backtrace)
;;       (setq cargo-process--enable-rust-backtrace t)
;;     (setq cargo-process--enable-rust-backtrace nil)))

;; (defun dia/rust-backtrace-string ()
;;   (interactive)
;;   (if (not cargo-process--enable-rust-backtrace)
;;       "Backtrace: disabled"
;;     "Backtrace: enabled"))

;; (defhydra rust-cargo-hydra ()
;;   "
;; %(dia/rust-backtrace-string)
;; "
;;   ("C-b" cargo-process-build "Build" :column "Cargo")
;;   ("C-r" cargo-process-run "Run")
;;   ("R" cargo-process-run-bin "Run (specific)")
;;   ("C-t" cargo-process-test "Test")
;;   ("c" cargo-process-clean "Clean")

;;   ("d" cargo-process-doc "Doc" :column "")
;;   ("D" cargo-process-doc-open "Doc (open)")
;;   ("u" cargo-process-update "Update")
;;   ("C" cargo-process-check "Check")
;;   ("a" cargo-process-audit "Audit")
;;   ("C-c" cargo-process-clippy "Clippy")

;;   ("n" next-error "Next" :column "Errors")
;;   ("N" next-error-skip-warnings "Next, skip warnings")
;;   ("p" previous-error "Previous")
;;   ("f" first-error "First")
;;   ("l" netrom/compilation-last-error "Last")
;;   ("k" kill-compilation "Stop")

;;   ("B" dia/toggle-rust-backtrace "Toggle backtrace" :column "Misc")
;;   ("q" nil "Cancel" :color blue))

;; (define-key rust-mode-map (kbd "C-c C-c C-h") 'rust-cargo-hydra/body)

;; (use-package rust-mode
;;   :ensure t
;;   ;; :after lsp-mode
;;   ;; :hook
;;   ;; (rust-mode . dia/rust-mode-hook)
;;   :init
;;   ;; (add-hook 'rust-mode-hook 'dia/rust-mode-hook)
;;   (dia/remove-mode-from-auto-mode-alist 'rustic-mode)
;;   :bind (:map
;;          rust-mode-map . (([?\t] . #'company-indent-or-complete-common)
;;                           ("C-c C-e" . #'lsp-rust-analyzer-expand-macro)))
;;   :config
;;   ;; (add-hook 'rust-mode-hook #'dia/rust-mode-hook)
;;   (setq rust-format-on-save t
;;         lsp-rust-analyzer-display-chaining-hints t
;;         lsp-rust-analyzer-display-parameter-hints t
;;         ;; lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand
;;         ;; lsp-rust-analyzer-macro-expansion-method 'lsp-rust-analyzer-macro-expansion-default
;;         lsp-rust-analyzer-macro-expansion-method 'dia/rust-analyzer-macro-expand
;;         ;;
;;         lsp-rust-analyzer-server-display-inlay-hints t
;;         lsp-rust-analyzer-proc-macro-enable t
;;         lsp-rust-analyzer-experimental-proc-attr-macros t ; experimental
;;         lsp-rust-full-docs t
;;         lsp-rust-server 'rust-analyzer))

(use-package rustic
    :ensure t
    ;; :after lsp-mode
    ;; :hook
    ;; (rust-mode . dia/rust-mode-hook)
    :init
    ;; (add-hook 'rust-mode-hook 'dia/rust-mode-hook)
    (dia/remove-mode-from-auto-mode-alist 'rust-mode)
    :bind (:map
           rustic-mode-map . (([?\t] . #'company-indent-or-complete-common)
                              ("C-c C-e" . #'lsp-rust-analyzer-expand-macro)))
    :config
    ;; (add-hook 'rust-mode-hook #'dia/rust-mode-hook)
    (setq rustic-format-on-save t
          ;; rustic-compile-display-method 'dia/rustic-compile-display
          lsp-rust-analyzer-display-chaining-hints t
          lsp-rust-analyzer-display-parameter-hints t
          ;; lsp-rust-analyzer-macro-expansion-method 'rustic-analyzer-macro-expand
          ;; lsp-rust-analyzer-macro-expansion-method 'lsp-rust-analyzer-macro-expansion-default
          lsp-rust-analyzer-macro-expansion-method 'dia/rust-analyzer-macro-expand
          ;;
          lsp-rust-analyzer-server-display-inlay-hints t
          lsp-rust-analyzer-proc-macro-enable t
          lsp-rust-analyzer-experimental-proc-attr-macros t ; experimental
          lsp-rust-full-docs t
          lsp-rust-server 'rust-analyzer))

(defun dia/rustic-compile-display (buf)
  (let ((found-win nil))
    (unless (dolist (win (window-list) found-win)
            (when (equal "*rustic-compilation*" (buffer-name (window-buffer win)))
              (setq found-win t)))
      (display-buffer-below-selected buf '(( window-height . 0.3))))))

(defun dia/rustic-mode-hook()
  (setq rustic-rustfmt-bin dia/rust-rustfmt-bin-v)
  ;; (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (setq max-mini-window-height 1)
  ;; (lsp)

  ;; (message "RUSTIC HOOK: Started Lsp??? Realy???")
  ;; (message "RUSTIC HOOK: Company backends now set to: %s" company-backends)
  ;; (message "RUSTIC HOOK: eldoc-mode is: %s eldoc-echo-area-use-multiline-p: %s max-mini-window-height: %s eldoc-echo-area-display-truncation-message: %s"
  ;;          eldoc-mode
  ;;          eldoc-echo-area-use-multiline-p
  ;;          max-mini-window-height
  ;;          eldoc-echo-area-display-truncation-message)
  ;; (message "RUSTIC HOOK: Company backends is set to: %s" company-backends)
  ;; (dia/lsp-after-init-hook)
  (message "RUSTIC HOOK: Company backends is set to: %s" company-backends)
  ;; (when (eq 'company-capf (car company-backends))
  ;;   (message "RUSTIC HOOK: company-capf in head of list")
  ;;   (setq company-backends (cdr company-backends)))
  )

;; (defun dia/rust-mode-hook()
;;   (setq rust-rustfmt-bin dia/rust-rustfmt-bin-v)
;;   (setq rustic-rustfmt-bin dia/rust-rustfmt-bin-v)
;;   (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
;;   (setq max-mini-window-height 1)
;;   (if (boundp 'cargo-minor-mode)
;;       (if cargo-minor-mode
;;           (message "Cargo minor mode bound and true")
;;         (message "Cargo minor mode bound and nil")
;;         (cargo-minor-mode 1))
;;     (message "Cargo minor mode not bound, try to start cargo-minor-mode")
;;     (cargo-minor-mode 1))
;;   (message "Try to print cargo-minor-mode: %s" cargo-minor-mode) ; (print cargo-minor-mode)
;;   (message "Starting Lsp")
;;   ;; (if company-backends
;;   ;;     (message "Company backends is NOT nil: %s" company-backends)
;;   ;;   (message "Company backends is nil: %s" company-backends)
;;   ;;   (push 'company-lsp company-backends)
;;   ;;   (message "Company backends set to: %s" company-backends))
;;   (message "Company backends is set to: %s" company-backends)
;;   (set (make-local-variable 'company-backends) nil)
;;   ;; (push 'company-lsp company-backends)
;;   (lsp)
;;   (message "Started Lsp??? Realy???")
;;   (message "Company backends now set to: %s" company-backends)
;;   (message ">>>>> eldoc-mode is: %s eldoc-echo-area-use-multiline-p: %s max-mini-window-height: %s eldoc-echo-area-display-truncation-message: %s"
;;            eldoc-mode
;;            eldoc-echo-area-use-multiline-p
;;            max-mini-window-height
;;            eldoc-echo-area-display-truncation-message)
;; )

;; (add-hook 'rust-mode-hook 'dia/rust-mode-hook)
(add-hook 'rustic-mode-hook 'dia/rustic-mode-hook)

;;; Use rustic-mode
;; (eval-when-compile
;;   (require 'setup-rustic-mode))

(provide 'setup-rust)
;;; setup-rust.el ends here
