;; (when window-system
;;    (if (not server-mode)
;;       (server-start nil t)))

(setq start-directory-path (getenv "PWD"))

(when window-system
  (add-to-list 'initial-frame-alist '(width . 210))
  (add-to-list 'initial-frame-alist '(height . 58))
  (add-to-list 'default-frame-alist '(width . 210))
  (add-to-list 'default-frame-alist '(height . 58))
  (add-to-list 'frame-inherited-parameters 'width)
  (add-to-list 'frame-inherited-parameters 'height)
  (setq inhibit-splash-screen t)	;; disable splash screan
  (setq inhibit-startup-message t)	;; ...
  ;; (setq split-height-threshold nil) ;; in window mode split horizontaly
  ;; (setq split-width-threshold 0)	;; ...
  (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)
  (tool-bar-mode -1)		;; disable tool bar
  (menu-bar-mode -1)            ;; disable menu bar
  (scroll-bar-mode -1)          ;; disable scroll bar
  )

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
(add-to-list 'package-archives
            '("melpa-milkbox" . "https://melpa.milkbox.net/packages/") t)
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

(defvar my-packages '(
                      bash-completion
                      zygospore
                      yasnippet
                      ws-butler
                      undo-tree         ; Undo (C-c u) with tree
                      swiper-helm       ;
                      sublime-themes    ;
                      sr-speedbar       ;
                      projectile        ; project tracker
                      popup-switcher    ; lib for popup menu creation
                      perl6-mode
                      magit             ; git
                      ivy               ; alternative for helm
                      highlight-parentheses ;
                      helm
                      helm-gtags
                      helm-projectile
                      git-blamed        ; coloze changes
                      ggtags            ; gtags analog
                      function-args     ; show function args
                      erlang
                      dtrt-indent       ; auto configure indent style
                      counsel
                      counsel-projectile
                      comment-dwim-2    ; advanced comment-dwim (M-;)
                      clang-format      ; use clang for code format
                      bug-hunter        ; hunting bugs in init.el, etc.
                      auto-complete     ; advanced AC
                      anything
                      iedit
                      volatile-highlights
                      anzu
                      company
                      company-quickhelp
                      clean-aindent-mode
                      paredit           ; auto pair
                      slime             ; part of lisp ide
                      ;; for golang
                      go-autocomplete   ; go autoomplete
                      flycheck
                      company-go        ;
                      multi-compile
                      go-eldoc
                      go-rename
                      go-dlv
                      ;;
                      swiper
                      swiper-helm
                      ;; debug
                      realgud
                      ;; SQL
                      sql-indent
                      expand-region
                      ;; Rust
                      rust-mode
                      ;; web
                      web-mode
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; place for my libs
(add-to-list 'load-path "~/.emacs.d/libs")
(add-to-list 'load-path "~/.emacs.d/setup")

(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

;; (load-if-exists "~/src/bitbucket.org/blais/beancount/src/elisp/beancount.el")

;; auto compile local libs
(byte-recompile-directory (expand-file-name "~/.emacs.d/setup") 0)
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
(require 'setup-go)
(require 'setup-org)
(require 'setup-rust)
(require 'setup-functions)
(require 'setup-php)
(require 'setup-sql)

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
;; (global-set-key (kbd "C-x r") 'helm-recentf)
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
;;
;; (define-key helm-command-map (kbd "C-x r j") 'jump-to-register)
;;
(global-set-key (kbd "M-g M-c") 'diabolo/goto-column)
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g M-g") 'diabolo/goto-line-and-column)
(global-set-key (kbd "C-c C-t") 'diabolo/transpose-buffers)

(defun diabolo-desktop-change-dir ()
  ""
  (interactive)
  (desktop-change-dir default-directory))

(defun diabolo-desktop-save ()
  ""
  (interactive)
  (desktop-save default-directory))

(global-set-key (kbd "\e\el") '(lambda () (interactive)
                                 (desktop-change-dir start-directory-path)
                                 (message "Loaded desktop file from %s" start-directory-path)))
(global-set-key (kbd "\e\es") '(lambda () (interactive)
                                 (desktop-save start-directory-path)
                                 (message "Saved desktop file to %s" start-directory-path)))

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
(set-register ?o (cons 'file "~/Documents/org/notes.org")) ;; open it with  C-x r j o

;;------------------------------------------------------------------------
;;
;; Custom variables (changed automatic)
;;
;;------------------------------------------------------------------------
;;
;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
;; '(default ((t (:family "Droid Sans Mono" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
;; '(default ((t (:family "Anonymous Pro Regular" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
;; '(default ((t (:family "Monospace" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
;; '(default ((t (:family "Source Code Pro Regular" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
;; '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "Inconsolata" :foundry "outline" :slant normal :weight bold :height 118 :width normal))))
;; '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 120 :width normal))))
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
    (expand-region sql-indent web-mode phpcbf web-beautify racer rust-mode php+-mode php-mode markdown-mode go-dlv realgud company-quickhelp go-rename go-autocomplete bash-completion perl6-mode comment-dwim-2 git-blamed helm-projectile erlang clang-format bug-hunter magit sr-speedbar swiper-helm sublime-themes ggtags function-args zygospore helm-gtags helm yasnippet ws-butler volatile-highlights use-package undo-tree iedit dtrt-indent counsel-projectile company clean-aindent-mode anzu)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1)))))
 '(tramp-syntax (quote default) nil (tramp)))
;; '(default ((t (:background nil))))
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
