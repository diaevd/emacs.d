(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories '("/usr/src/zen/zen-kernel"))
 '(editorconfig-mode t)
 '(eldoc-echo-area-use-multiline-p 1)
 '(helm-completion-style 'emacs)
 '(lsp-auto-execute-action nil)
 '(lsp-auto-guess-root t)
 '(lsp-eldoc-render-all nil)
 '(lsp-headerline-breadcrumb-enable t)
 '(lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))
 '(lsp-keymap-prefix "C-c C-l")
 '(lsp-lens-enable t)
 '(lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
 '(lsp-rust-analyzer-display-chaining-hints t t)
 '(lsp-rust-analyzer-display-parameter-hints t t)
 '(lsp-rust-analyzer-inlay-face 'lsp-face-semhl-label)
 '(lsp-rust-analyzer-macro-expansion-method 'dia/rust-analyzer-macro-expand t)
 '(lsp-rust-analyzer-proc-macro-enable t)
 '(lsp-rust-analyzer-rustfmt-extra-args [])
 '(lsp-rust-analyzer-rustfmt-override-command ["rustup run stable rustfmt"])
 '(lsp-rust-analyzer-server-display-inlay-hints t t)
 '(lsp-rust-unstable-features t)
 '(lsp-semantic-tokens-enable t)
 '(lsp-treemacs-sync-mode t)
 '(lsp-typescript-implementations-code-lens-enabled t)
 '(lsp-typescript-references-code-lens-enabled t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 15)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-flycheck-list-position 'bottom)
 '(lsp-ui-sideline-actions-icon "~/.emacs.d/icons/idea2-256x256.png")
 '(lsp-ui-sideline-diagnostic-max-line-length 60)
 '(lsp-ui-sideline-diagnostic-max-lines 3)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-hover nil)
 '(lsp-ui-sideline-show-symbol t)
 '(org-agenda-files
   '("~/Documents/org/notes.org" "~/Documents/org/work.org" "~/Documents/org/home.org" "~/Documents/org/links.org" "~/Documents/org/daily.org"))
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-directory "~/Documents/org")
 '(org-indent-mode t)
 '(org-log-done t)
 '(org-return-follows-link t)
 '(org-startup-folded nil)
 '(package-selected-packages
   '(helpful treemacs-projectile company-box company-posframe helm-posframe posframe rustic toml-mode use-package-ensure-system-package system-packages zygospore yaml-mode ws-butler web-mode web-beautify vue-mode volatile-highlights use-package undo-tree swiper-helm sublime-themes sr-speedbar sqlup-mode sql-indent slime realgud racer popup-switcher phpcbf php-eldoc php-auto-yasnippets php+-mode perl6-mode paredit multi-compile magit lsp-ui iedit highlight-parentheses helm-swoop helm-projectile helm-gtags go-rename go-eldoc go-dlv go-autocomplete git-blamed ggtags function-args flycheck-rust expand-region erlang editorconfig dtrt-indent diminish counsel-projectile company-quickhelp company-php company-lsp company-go company-c-headers comment-dwim-2 clean-aindent-mode clang-format cargo bug-hunter bash-completion auto-package-update anzu anything))
 '(rustic-lsp-server 'rust-analyzer)
 '(safe-local-variable-values
   '(eval
     (when
         (require 'rainbow-mode nil t)
       (rainbow-mode 1))))
 '(tramp-syntax 'default nil (tramp)))

;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background nil))))
;;  ;; '(default ((t (:family "Anonymous Pro Regular" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;;  '(cperl-array-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;;  '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;;  '(fa-face-hint ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;;  '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(lsp-face-semhl-label ((t (:inherit font-lock-comment-face :foreground "dim gray"))))
 '(lsp-ui-doc-background ((t (:background "#272A36" :family "Anonymous Pro"))))
 '(lsp-ui-doc-header ((t (:background "deep sky blue" :foreground "black" :family "Anonymous Pro"))))
 '(lsp-ui-doc-url ((t (:inherit link :family "Anonymous Pro"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "yellow" :family "Anonymous Pro"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "white" :box (:line-width (1 . -1) :color "white") :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-global ((t (:foreground "deep sky blue" :family "Anonymous Pro")))))
