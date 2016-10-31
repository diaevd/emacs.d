;; in window session disable menubar
(when window-system (menu-bar-mode -1))
(tool-bar-mode -1)
;; Display the name of the current buffer in the title bar
(setq frame-title-format "%b")
;; (global-set-key [f9] 'menu-bar-open) ;; Sometimes need (F10 by default)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(setq-default major-mode 'text-mode)
(setq column-number-mode t)
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
              indent-tabs-mode 't
              c-default-style "linux")

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
(setq windmove-wrap-around t )

;; company
(use-package company
  :init
  (global-company-mode 1)
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
(add-hook 'c-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
(add-hook 'c++-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
(add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'c-mode-hook
;; 	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(global-set-key (kbd "C-c C-w") 'delete-trailing-whitespace)
(windmove-default-keybindings)

(provide 'setup-general)
