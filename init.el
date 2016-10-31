(when window-system (add-to-list 'default-frame-alist '(width . 208))
      (add-to-list 'default-frame-alist '(height . 56))
      (setq inhibit-splash-screen t)	;; disable splash screan
      (setq inhibit-startup-message t)	;; ...
      ;; (setq split-height-threshold nil) ;; in window mode split horizontaly
      ;; (setq split-width-threshold 0)	;; ...
      (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)
      (tool-bar-mode -1)		;; disable tool bar
      )

(defun 2-windows-vertical-to-horizontal ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))
;; (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

;;------------------------------------------------------------------------
;;
;; MELPA
;;
;;------------------------------------------------------------------------
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
            '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize) ;; You might already have this line

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; MELPA use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; place for my libs
(add-to-list 'load-path "~/.emacs.d/libs")
(add-to-list 'load-path "~/.emacs.d/custom")

;;------------------------------------------------------------------------
;;
;; auto complete mode
;;
;;------------------------------------------------------------------------
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

;;------------------------------------------------------------------------
;;
;; BASH
;;
;;------------------------------------------------------------------------
(require 'bash-completion)
(bash-completion-setup)
(add-to-list 'auto-mode-alist '("[^_[:alnum:]|[:space:]]\\.[^.]\\w+\\'" . sh-mode))
(global-set-key [f6] 'shell)

(byte-recompile-directory (expand-file-name "~/.emacs.d/custom") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/libs") 0)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/themes") 0)

(load "profile-dotemacs")
(defvar profile-dotemacs-file "~/.emacs.d/init.el" "File to be profiled.")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-themes)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-bash)
(require 'setup-clang)
(require 'setup-perl)
(require 'setup-hideshow)
(require 'setup-mql)
(require 'setup-erlang)
(require 'setup-autocomplete)
(require 'setup-popup-switcher)
(require 'setup-git)
(require 'setup-functions)

;;------------------------------------------------------------------------
;;
;; All global keys place here because maybe currupted by prev setups
;;
;;------------------------------------------------------------------------
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r") 'helm-recentf)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-h o") 'helm-occur)
(global-set-key (kbd "C-h w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-h g") 'helm-google-suggest)
(global-set-key (kbd "C-h x") 'helm-register)
;;
(global-set-key (kbd "C-M-k") 'kill-sexp)
;;
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c v") 'imenu-tree)
(global-set-key (kbd "C-c j") 'ffap)
(setq imenu-tree-auto-update 't)
(setq imenu-auto-rescan 't)
(eval-after-load "imenu"
  '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))

;;------------------------------------------------------------------------
;;
;; Registers to open files
;;
;;------------------------------------------------------------------------
;;;; (global-set-key (kbd "<f6>") (lambda() (interactive) (find-file "~/.emacs")))
(set-register ?e (cons 'file "~/.emacs.d/init.el")) ;; open it with  C-x r j e
(set-register ?l (cons 'file "~/Documents/org/links.org")) ;; open it with  C-x r j l
(set-register ?o (cons 'file "~/Documents/org/notes.org")) ;; open it with  C-x o j 0

;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/usr/src/zen/zen-kernel")))
 '(org-agenda-files
   (quote
    ("~/Documents/org/notes.org" "~/Documents/org/work.org" "~/Documents/org/home.org" "~/Documents/org/links.org" "~/Documents/org/daily.org")))
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-directory "~/Documents/org")
 '(org-indent-mode t)
 '(org-log-done t)
 '(org-return-follows-link t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (git-blamed helm-projectile erlang clang-format bug-hunter magit sr-speedbar swiper-helm sublime-themes ggtags function-args zygospore helm-gtags helm yasnippet ws-butler volatile-highlights use-package undo-tree iedit dtrt-indent counsel-projectile company clean-aindent-mode anzu)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Anonymous Pro" :foundry "outline" :slant normal :weight bold :height 120 :width normal))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold)))))
