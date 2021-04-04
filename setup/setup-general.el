;; in window session disable menubar
(when window-system (menu-bar-mode -1))
(tool-bar-mode -1) ;; disable toolbar
(scroll-bar-mode -1) ;; disable scroll bars
;; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")
;; (global-set-key [f9] 'menu-bar-open) ;; Sometimes need (F10 by default)
;; '(default ((t (:family "Droid Sans Mono" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
;; '(default ((t (:family "Anonymous Pro Regular" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
;; '(default ((t (:family "Monospace" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
;; '(default ((t (:family "Source Code Pro Regular" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
;; '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "Inconsolata" :foundry "outline" :slant normal :weight bold :height 118 :width normal))))
;; '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 120 :width normal))))
;; (set-default-font "Hack-12")
;; (set-default-font "Droid Sans Mono-12")
;; (set-face-attribute 'default t :family "Hack")
(setq-default user-full-name   "Evgeny Duzhakov"
              user-mail-adress "diaevd@gmail.com")

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Перечитываем измененные файлы
(global-auto-revert-mode t)
;; в жопу eldoc в глобале
(global-eldoc-mode -1)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(setq-default major-mode 'text-mode)

(setq column-number-mode t)
;; fix column number format for starting from 1
(setq mode-line-position
      '("%p (%l," (:eval (format "%d)" (1+ (current-column))))))
;; force the update of the mode line so the column gets updated
(add-hook 'post-command-hook 'force-mode-line-update)
;; (setq-default truncate-lines 1) ;; no wordwrap

;; doom-modeline игнорит падла mode-line-position
(setq column-number-indicator-zero-based nil)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)  ;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir
(setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;; where to save auto-replace maps
;; Use desktop-mode only in window-mode
;; (when window-system (progn
;;                       (setq desktop-load-locked-desktop "ask") ;; sometimes desktop is locked, ask if we want to load it.
;;                       (desktop-save-mode 1) ;; auto-save buffer state on close for a later time.
;;                       ))

;; colorize the output of the compilation mode.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))

  ;; mocha seems to output some non-standard control characters that
  ;; aren't recognized by ansi-color-apply-on-region, so we'll
  ;; manually convert these into the newlines they should be.
  (goto-char (point-min))
  (while (re-search-forward "\\[2K\\[0G" nil t)
    (progn
      (replace-match "
")))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'linum)

;; use space to indent by default
(setq-default indent-tabs-mode 't)
;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 8)
(setq indent-line-function 'insert-tab)
(setq sh-basic-offset 8)
(setq sh-indentation 8)

(setq-default c-basic-offset 8
              c-indentation 8
              c-default-style "linux")

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 8)))

;; Compilation Support
;; C-o Display matched location, but do not switch point to matched buffer
;; M-n Move to next error message, but do not visit error location
;; M-p Move to next previous message, but do not visit error location
;; M-g n Move to next error message, visit error location
;; M-g p Move to previous error message, visit error location
;; RET Visit location of error at poiint
;; M-{ Move point to the next error message or match occurring in a different file
;; M-} Move point to the previous error message or match occurring in a different file
;; q Quit *compilation* buffer
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
;; Compilation output
(setq compilation-scroll-output t)
;; (setq compilation-scroll-output 'first-error) ;; scroll to first error

;; Debugging
;; 1. GUD interaction buffer
;; 2. Locals/Registers buffer
;; 3. Primary Source buffer
;; 4. I/O buffer for debugging program
;; 5. Stack buffer6. Breakpoints/Threads buffer
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq auto-window-vscroll nil) ;; disable auto scrolling
(setq-default scroll-margin 1)
(setq-default scroll-step 1) ;; keyboard scroll one line at a time
(setq-default scroll-conservatively 0) ;; lags fix (like ssh sessions)
(setq-default scroll-up-aggressively 0.01)
(setq-default scroll-down-aggressively 0.01)

;; shift+arrows_keys is for select by default, however change it so
;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)
;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t)

;; pos-tip fixes popups https://github.com/pitkali/pos-tip
(require 'pos-tip)

;;------------------------------------------------------------------------
;;
;; auto complete mode
;;
;;------------------------------------------------------------------------
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

;; company
(use-package company
  :init
  (global-company-mode 1)
  (company-quickhelp-mode 1)
  (add-to-list 'company-backends 'php-extras-company t)
  (delete 'company-semantic company-backends))
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; Package: projejctile
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  ;; (setq projectile-switch-project-action 'projectile-dired)
  ;; (setq projectile-switch-project-action 'projectile-find-dir)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-enable-caching t)
  (hack-dir-local-variables-non-file-buffer)
  (hack-local-variables))
(add-hook 'after-change-major-mode-hook 'hack-local-variables)

;; fix for .dir-locals.el
(defun ketbra/projectile-switch-project-action ()
  "Run projectile-find-file with dir-locals properly set."
  (with-temp-buffer
    (setq default-directory project-to-switch)
    (hack-dir-local-variables-non-file-buffer)
    (funcall switch-project-action)))
(setq projectile-switch-project-action #'ketbra/projectile-switch-project-action)

;; Package zygospore
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; delete trailing white space
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
(add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
(add-hook 'mql-mode-hook (lambda () (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'c-mode-hook
;; 	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(global-set-key (kbd "C-c C-w") 'delete-trailing-whitespace)
(windmove-default-keybindings)

(provide 'setup-general)
