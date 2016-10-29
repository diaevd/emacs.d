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

(add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-c C-b") 'mql-compile-dispatcher)))
;; (add-hook 'mql-mode-hook 'turn-on-orgtbl)
(provide 'setup-mql)
