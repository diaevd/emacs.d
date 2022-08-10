;; (when window-system
;;    (if (not server-mode)
;;       (server-start nil t)))

(when (boundp 'server-mode) (message "!>> server-mode: %s" server-mode))
(when (boundp 'server-process) (message "!>> server-process: %s" server-process))
(when (boundp 'windows-system) (message "!>> windows-system: %s" window-system))

;;;(setq user-emacs-directory "~/emacs.d")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; (add-hook 'kill-emacs-query-functions 'custom-prompt-customize-unsaved-options)

(setq start-directory-path (getenv "PWD"))

(setq inhibit-splash-screen t)	 ;; disable splash screan
(setq inhibit-startup-message t) ;; ...
(tool-bar-mode -1)		 ;; disable tool bar
(menu-bar-mode -1)               ;; disable menu bar
(scroll-bar-mode -1)             ;; disable scroll bar

(setq initial-frame-alist
      '((left . 1)
        (top . 1)
        (width . 211)
        (height . 57)
        (menu-bar-lines . 0)))

(defun 2-windows-vertical-to-horizontal ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
     (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))

(when window-system
  (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal))

;; Just for debug
(defun dia/before-make-frame-hook ()
  (message ">>> Enter before-make-frame-hook...")
  (message "default-frame-alist: %s" default-frame-alist)
  (message "initial-frame-alist: %s" initial-frame-alist)
  (message "window-system-default-frame-alist: %s" window-system-default-frame-alist)
  (message "frame-width: %s" (frame-width))
  (message "frame-height: %s" (frame-height))
  (message "frame-initial-frame: %s" frame-initial-frame)
  (message "frame-initial-geometry-arguments: %s" frame-initial-geometry-arguments)
  (when (boundp 'server-mode) (message "> server-mode: %s" server-mode))
  (when (boundp 'server-process) (message "> server-process: %s" server-process))
  )

(defun dia/after-make-frame-hook (new_frame)
;;   (when (boundp 'server-process)
  (message ">>> Enter after-make-frame-functions...")
  (message "frame: $s" new_frame)
  (message "default-frame-alist: %s" default-frame-alist)
  (message "> frame-width: %s" (frame-width new_frame))
  (message "> frame-height: %s" (frame-height new_frame))
  (message "> frame-geometry: %s" (frame-geometry new_frame))
  (message "frame-initial-frame: %s" frame-initial-frame)
  (message "frame-initial-geometry-arguments: %s" frame-initial-geometry-arguments)
  (when (boundp 'server-mode) (message "> server-mode: %s" server-mode))
  (when (boundp 'server-process) (message "> server-process: %s" server-process))
  (when (bound-and-true-p server-process)
    (message "!!! Started as client, try to set frame")
    (dia/resize-by-initial-frame-alist-or-props '((left . 1)
                                                  (top . 1)
                                                  (width . 164)
                                                  (height . 54)
                                                  (menu-bar-lines . 0))
                                                )
    t)
  )

(add-hook 'before-make-frame-hook #'dia/before-make-frame-hook)
(add-hook 'after-make-frame-functions #'dia/after-make-frame-hook)

;; (when nil ;; window-system
;;   ;; Anonymous Pro
;;   (add-to-list 'initial-frame-alist '(left . 0))
;;   (add-to-list 'initial-frame-alist '(top . 0))
;;   (add-to-list 'initial-frame-alist '(width . 211))
;;   (add-to-list 'initial-frame-alist '(height . 58))
;;   (add-to-list 'default-frame-alist '(left . 0))
;;   (add-to-list 'default-frame-alist '(top . 0))
;;   (add-to-list 'default-frame-alist '(width . 211))
;;   (add-to-list 'default-frame-alist '(height . 58))
;;   ;; Iosevka SS02
;;   ;; (add-to-list 'initial-frame-alist '(width . 237))
;;   ;; (add-to-list 'initial-frame-alist '(height . 50))
;;   ;; (add-to-list 'default-frame-alist '(width . 237))
;;   ;; (add-to-list 'default-frame-alist '(height . 50))
;;   (add-to-list 'frame-inherited-parameters 'width)
;;   (add-to-list 'frame-inherited-parameters 'height)
;;   (setq inhibit-splash-screen t)	;; disable splash screan
;;   (setq inhibit-startup-message t)	;; ...
;;   ;; (setq split-height-threshold nil) ;; in window mode split horizontaly
;;   ;; (setq split-width-threshold 0)	;; ...
;;   (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)
;;   (tool-bar-mode -1)		;; disable tool bar
;;   (menu-bar-mode -1)            ;; disable menu bar
;;   (scroll-bar-mode -1)          ;; disable scroll bar
;;   )

;;------------------------------------------------------------------------
;;
;; Unbind unneeded keys
;;
;;------------------------------------------------------------------------
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "C-x C-z"))

;;------------------------------------------------------------------------
;;
;; Straight
;;
;;------------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-use-package 'async)

;;------------------------------------------------------------------------
;;
;; MELPA
;;
;;------------------------------------------------------------------------
(eval-and-compile
  (setq load-prefer-newer t
        use-package-always-ensure t
        use-package-verbose t))

(straight-use-package 'bind-key)
(straight-use-package 'bug-hunter)

;; Manage your installed packages with emacs
;; https://github.com/jabranham/system-packages
(use-package system-packages)
(use-package use-package-ensure-system-package)

;; for font-lock+
;; (use-package quelpa
;;   :defer nil
;;   :config
;;   (quelpa
;;    '(quelpa-use-package
;;      :fetcher git
;;      :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
;;   (require 'quelpa-use-package))

;; (use-package font-lock+
;;   :requires (quelpa quelpa-use-package)
;;   :quelpa
;;   (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

;; place for my libs
(add-to-list 'load-path (expand-file-name "libs" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "straight/repos/helm" user-emacs-directory))
;; (add-to-list 'load-path "~/.local/share/icons-in-terminal")

(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

;; (load-if-exists "~/src/bitbucket.org/blais/beancount/src/elisp/beancount.el")

;; auto compile local libs
;; (byte-recompile-directory (expand-file-name "setup" user-emacs-directory) 0)
(byte-recompile-directory (expand-file-name "libs" user-emacs-directory) 0)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/themes") 0)

(load "profile-dotemacs")
(defvar profile-dotemacs-file (expand-file-name "init.el" user-emacs-directory) "File to be profiled.")

;; (straight-use-package 'gas-mode)
(straight-use-package 'riscv-mode)
(straight-use-package 'mips-mode)
(straight-use-package 's12cpuv2-mode)

;; (eval-and-compile
  (require 'setup-functions)
  (require 'setup-general)
  (require 'setup-lisp)
  (require 'setup-helm)
  ;; (require 'setup-helm-gtags)
  ;; (require 'setup-ggtags)
  (require 'setup-themes)
  ;; (require 'setup-font-firacode)          ; firacode font
  ;; (require 'setup-font-firacode2)          ; firacode font v2
  (require 'setup-cedet)
  (require 'setup-editing)
  (require 'setup-bash)
  (require 'setup-clang)
  ;; (require 'setup-perl)
  (require 'setup-hideshow)
  (require 'setup-mql)
  ;;; (require 'setup-erlang)
  (require 'setup-autocomplete)
  (require 'setup-popup-switcher)
  (require 'setup-git)
  ;; (require 'setup-go)
  (require 'setup-org)
  ;; (require 'setup-lsp-new)
  ;; (require 'setup-lsp)
  (require 'setup-rust)
  (require 'setup-shackle)
  ;; (require 'setup-maple-minibuffer)
  ;; (require 'setup-web)
  ;; (require 'setup-sql)
;; )

;;------------------------------------------------------------------------
;;
;; All global keys place here because maybe currupted by prev setups
;;
;;------------------------------------------------------------------------

;;
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
(global-set-key (kbd "\e\ea") 'dtrt-indent-adapt)
;; Helpful
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h c") 'helpful-command)
;; elisp
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)

;;
;; (define-key helm-command-map (kbd "C-x r j") 'jump-to-register)
;;
(global-set-key (kbd "M-g M-c") 'dia/goto-column)
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g M-g") 'dia/goto-line-and-column)
(global-set-key (kbd "C-c C-t") 'dia/transpose-buffers)

(require 'desktop) ;this line is needed.
(push '(company-posframe-mode . nil) desktop-minor-mode-table)

(defun dia/desktop-change-dir ()
  ""
  (interactive)
  (desktop-change-dir default-directory))

(defun dia/desktop-save ()
  ""
  (interactive)
  (desktop-save default-directory))

(global-set-key (kbd "\e\el") (lambda () (interactive)
                                 (desktop-change-dir start-directory-path)
                                 (message "Loaded desktop file from %s" start-directory-path)))
(global-set-key (kbd "\e\es") (lambda () (interactive)
                                 (desktop-save start-directory-path)
                                 (message "Saved desktop file to %s" start-directory-path)))

(setq imenu-tree-auto-update 't)
(setq imenu-auto-rescan 't)
;;;(eval-after-load "imenu"
;;;  '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))

;;------------------------------------------------------------------------
;;
;; Registers to open files
;;
;;------------------------------------------------------------------------
;;;; (global-set-key (kbd "<f6>") (lambda() (interactive) (find-file "~/.emacs")))
(set-register ?e (cons 'file (expand-file-name "init.el" user-emacs-directory))) ;; open it with  C-x r j e
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

;;; CUSTOM VARIABLES PLACES

;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:background nil))))
 ;; '(default ((t (:family "Anonymous Pro Regular" :foundry "outline" :slant normal :weight bold :height 11)))))
;; (set-face-attribute 'default t
;;                     :family "Anonymous Pro Regular")
;; (set-face-attribute 'default t
;;                     :foundry "outline")
;; (set-face-attribute 'default t
;;                     :slant normal)
;; (set-face-attribute 'default t
;;                     :weight bold)
;; (set-face-attribute 'default t
;;                     :height 11)
