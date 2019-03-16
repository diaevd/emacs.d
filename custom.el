(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories '("/usr/src/zen/zen-kernel"))
 '(eldoc-echo-area-use-multiline-p nil)
 '(org-agenda-files
   '("~/Documents/org/notes.org" "~/Documents/org/work.org" "~/Documents/org/home.org" "~/Documents/org/links.org" "~/Documents/org/daily.org"))
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-directory "~/Documents/org")
 '(org-indent-mode t)
 '(org-log-done t)
 '(org-return-follows-link t)
 '(org-startup-folded nil)
 '(package-selected-packages
   '(rustic eglot toml-mode use-package-ensure-system-package system-packages zygospore yaml-mode ws-butler web-mode web-beautify vue-mode volatile-highlights use-package undo-tree swiper-helm sublime-themes sr-speedbar sqlup-mode sql-indent slime realgud racer popup-switcher phpcbf php-eldoc php-auto-yasnippets php+-mode perl6-mode paredit multi-compile magit lsp-ui lsp-rust iedit highlight-parentheses helm-swoop helm-projectile helm-gtags go-rename go-eldoc go-dlv go-autocomplete git-blamed ggtags function-args flycheck-rust expand-region erlang editorconfig dtrt-indent diminish counsel-projectile company-quickhelp company-php company-lsp company-go company-c-headers comment-dwim-2 clean-aindent-mode clang-format cargo bug-hunter bash-completion auto-package-update anzu anything))
 '(safe-local-variable-values
   '((eval when
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
 '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold)))))
