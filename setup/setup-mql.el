;;------------------------------------------------------------------------
;;
;; MQL
;;
;;;; -- https://github.com/kostafey/kostafeys-emacs-confik/blob/master/artifacts/mql-mode.el
;; https://github.com/jianingy/emacs-ysl/blob/master/site-lisp/extra/mql-mode.el
;;------------------------------------------------------------------------
;;(load "~/.emacs.d/mql-mode.el")
(require 'mql-mode)
(modify-coding-system-alist 'file "\\.mql\\'" 'windows-1251)
(modify-coding-system-alist 'file "\\.mq4\\'" 'windows-1251)
(modify-coding-system-alist 'file "\\.mq5\\'" 'windows-1251)
(modify-coding-system-alist 'file "\\.mqh\\'" 'windows-1251)

(font-lock-add-keywords 'mql-mode
			'(("input" . 'font-lock-keyword-face)
			  ("sinput" . 'font-lock-keyword-face)))
(font-lock-add-keywords 'mql-mode
			'(("SEEK_CUR" . 'font-lock-builtin-face)
			  ("SEEK_END" . 'font-lock-builtin-face)
			  ("SEEK_SET" . 'font-lock-builtin-face)))
(font-lock-add-keywords 'mql-mode
			'(("FileIsExist" . 'font-lock-function-name-face)
			  ("FolderCreate" . 'font-lock-function-name-face)
			  ("ResetLastError" . 'font-lock-function-name-face)
			  ("StringSplit" . 'font-lock-function-name-face)
			  ("StringToInteger" . 'font-lock-function-name-face)))

(eval-after-load "semantic"
  '(add-to-list 'semantic-new-buffer-setup-functions (cons 'mql-mode 'semantic-default-c-setup)))
(define-child-mode mql-mode c++-mode
  "`mql-mode' uses the same parser as `c++-mode'.")
(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".mqh")
     (speedbar-add-supported-extension ".mql")
     (speedbar-add-supported-extension ".mq4")
     (speedbar-add-supported-extension ".mq5")))

(add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-c C-b") 'mql-compile-dispatcher)))
;; (add-hook 'mql-mode-hook 'turn-on-orgtbl)
(provide 'setup-mql)
