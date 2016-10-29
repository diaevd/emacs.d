;;
;; HideShow
;;
;; Rebinding keys for hideshow
(require 'hideshow)
;; (hs-minor-mode t)
(define-key hs-minor-mode-map "\C-c\C-o"
  (let ((map (lookup-key hs-minor-mode-map "\C-c@")))
    ;; C-h is help to remind me key binding
    ;; (define-key map "\C-h" 'describe-prefix-bindings)
    (define-key map "\C-q" 'hs-toggle-hiding)
    ;; compatible with outline
    (define-key map "\C-c" 'hs-hide-block)
    (define-key map "\C-e" 'hs-show-block)
    map))
;; (global-set-key (kbd "C-c q") 'hs-toggle-hiding)
;; (global-set-key (kbd "C-c c") 'hs-hide-block)
;; (global-set-key (kbd "C-c e") 'hs-show-block)

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
	 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
	      (hs-toggle-hiding)
	    (error t))
	  (hs-show-all))
    (toggle-selective-display column)))

;; (load-library "hideshow")
;; (global-set-key (kbd "M-+") 'toggle-hiding)
(global-set-key (kbd "M-+") 'toggle-hiding)
(global-set-key (kbd "\e\e=") 'toggle-hiding)
(global-set-key (kbd "\e\e-") 'hs-hide-all)
(global-set-key (kbd "\e\e+") 'hs-show-all)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'mql-mode-hook        'hs-minor-mode)

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)
;; Add more here
(provide 'setup-hideshow)
