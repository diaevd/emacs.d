;;------------------------------------------------------------------------
;;
;; MQL
;;
;;;; -- https://github.com/kostafey/kostafeys-emacs-confik/blob/master/artifacts/mql-mode.el
;; https://github.com/jianingy/emacs-ysl/blob/master/site-lisp/extra/mql-mode.el
;;------------------------------------------------------------------------
;;(load "~/.emacs.d/mql-mode.el")

(add-to-list 'load-path "~/.emacs.d/libs")
(eval-when-compile (require 'mql-mode))
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
                          ("FileWriteStruct" . 'font-lock-function-name-face)
                          ("FileReadStruct" . 'font-lock-function-name-face)
			  ("FolderCreate" . 'font-lock-function-name-face)
			  ("ResetLastError" . 'font-lock-function-name-face)
			  ("StringSplit" . 'font-lock-function-name-face)
			  ("StringToInteger" . 'font-lock-function-name-face)))

(eval-after-load "semantic"
  '(progn
     (add-to-list 'semantic-new-buffer-setup-functions
                  (cons 'mql-mode 'semantic-default-c-setup))
     (define-child-mode mql-mode c++-mode
       "`mql-mode' uses the same parser as `c++-mode'.")))

(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".mqh")
     (speedbar-add-supported-extension ".mql")
     (speedbar-add-supported-extension ".mq4")
     (speedbar-add-supported-extension ".mq5")))

(add-hook 'mql-mode-hook (lambda ()
                           (local-set-key (kbd "C-c C-b") 'mql-compile-dispatcher)))

;; Error cheking
(eval-when-compile (require 'compile))

;; Catch errors like "FastBot.mq4(30,1) : error 149: 'extern' - unexpected token"
(add-hook 'mql-mode-hook (lambda ()
                           (add-to-list 'compilation-error-regexp-alist
                                        '("^\\([[:alnum:] _/\]+\\.mq[hl45]\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)) : error [[:digit:]]+:.+$"
                                          1 2 3 2))
                           (add-to-list 'compilation-error-regexp-alist
                                        '("^\\([[:alnum:] _/\]+\\.mq[hl45]\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)) : warning [[:digit:]]+:.+$"
                                          1 2 3 1))
                           ))
;; (add-hook 'mql-mode-hook 'turn-on-orgtbl)
(provide 'setup-mql)
