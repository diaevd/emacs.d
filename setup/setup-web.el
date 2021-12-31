;;; setup-web.el --- WEB Config

;; Copyright (C) 2017 Free Software Foundation, Inc.
;;
;; Author: Evgeny Duzhakov <diabolo@veles>
;; Maintainer: Evgeny Duzhakov <diabolo@veles>
;; Created: 10 Jul 2017
;; Version: 0.02
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'setup-web)

;;; Code:


;; (require 'php-beautifier)

;; (setq php-beautifier-executable-path "/usr/bin/php_beautifier")
;; (setq php-beautifier-indent-method "tabs")

;; (setq php-beautifier-phpcbf-path "/usr/bin/phpcbf")
;; PHPCS, PSR1, MySource, Squiz, PSR2, Zend and PEAR
;; (setq php-beautifier-phpcbf-standard "PSR2")

(use-package flycheck
  :config
  (flycheck-define-checker my-php
    "A PHP syntax checker using the PHP command line interpreter.
See URL `http://php.net/manual/en/features.commandline.php'."
    :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
              "-d" "log_errors=0" source)
    :error-patterns
    ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
            (message) " in " (file-name) " on line " line line-end))
    :modes (web-mode php-mode)))

;; TODO: use yasnippet to do this
;; (defun insert-php-doc-comment ()
;;   "To insert doc comments on php functions."
;;   (interactive)
;;   (insert "/**\n * TODO add description. \n * \n * @param \n * @return \n */"))

;; (add-hook 'web-mode-hook (lambda () (local-set-key [(S-f1)] 'insert-php-doc-comment)))

(defun my-web-mode-hook ()
  ;; ""
  ;; (web-mode)
  (local-set-key (kbd "M-;") 'comment-dwim-2)
  (setq comment-multi-line nil ;; maybe
        comment-start "// "
        comment-end ""
        comment-style 'indent
        comment-use-syntax t)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (local-set-key [(S-f1)] 'insert-php-doc-comment)

  (local-set-key (kbd "C-c C-w") 'delete-trailing-whitespace)
  (setq web-mode-comment-formats
        '(("java"       . "//")
          ("javascript" . "//")
          ("php"        . "//")
          ("jsx"        . "//")
          ))

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  ;; (use-package php-auto-yasnippets)
  ;; (require 'php-auto-yasnippets)
  ;; (local-set-key (kbd "C-c C-y") 'yas/create-php-snippet)

  ;; (flymake-mode-on)
  (flycheck-mode nil)
  (flycheck-select-checker 'my-php)
  )
;; (add-hook 'web-mode-hook (lambda () (local-set-key (kbd "M-;") 'comment-dwim-2)))
;; (add-hook 'web-mode-hook (lambda () (setq comment-multi-line nil ;; maybe
;;                                           comment-start "// "
;;                                           comment-end ""
;;                                           comment-style 'indent
;;                                           comment-use-syntax t)))
;; (add-hook 'web-mode-hook (lambda () (flycheck-mode t)))
;; (add-hook 'web-mode-hook (lambda () (flymake-mode-on)))

;; (require 'php-cs-fixer)

;; php documentation
; (require 'php-eldoc)
; (add-hook 'php-mode-hook 'eldoc-mode)

(use-package company-php
  :ensure t)

;; (add-hook 'web-mode-hook
;; 	  (lambda ()
;; 	    (set (make-local-variable 'company-backends)
;; 		 '((php-extras-company company-dabbrev-code) company-capf company-files))))


;; set psr-2 coding style
;; (add-hook 'web-mode-hook 'php-enable-psr2-coding-style)
 ;; (setq flycheck-phpcs-standard "PSR2")

;; php-auto-yasnippet
;; (use-package php-auto-yasnippets)
;; (require 'php-auto-yasnippets)

;; php-extras
;; (use-package php-extras
;; :ensure t
  ;; :defer t)

;; https://github.com/prathamesh-sonpatki/dotemacs/blob/master/hooks/web.el
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tmpl$" . web-mode)) ;;
  (add-to-list 'auto-mode-alist '("\\.tpl$" . web-mode))  ;;
  (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))  ;;
  (add-hook 'web-mode 'vue-mode)
  (add-hook 'web-mode 'vue-html-mode)
  ;;(add-hook 'vue-html-mode 'emmet-mode)

  (add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
  (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
  (add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
  (add-to-list 'auto-mode-alist '("\\.ts\\'"     . web-mode))       ;; TypeScript
  (add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
  (add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS
  (add-to-list 'auto-mode-alist '("\\.php\\'"    . web-mode))       ;; PHP
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template

  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("javascript" . "\\.es6?\\'")))

  (setq web-mode-engines-alist
        '(("blade"  . "\\.blade\\.")))

  ;; (setq web-mode-markup-indent-offset 2)
  ;; (setq web-mode-css-indent-offset 2)
  ;; (setq web-mode-code-indent-offset 2)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "js")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)

  (add-hook 'web-mode-hook 'my-web-mode-hook)

  (add-hook 'web-mode-hook (lambda ()
			     (local-set-key (kbd "C-c C-y") 'yas/create-php-snippet)
                             (setq editorconfig-exec-path "/usr/bin/editorconfig")
                             (add-hook 'editorconfig-custom-hooks
	                               (lambda (hash) (setq web-mode-block-padding 0)))
			     (editorconfig-mode t)
                             (editorconfig-apply)
                             (lambda (hash) (setq web-mode-block-padding 0)))))

(provide 'setup-web)
;;; setup-php.el ends here
