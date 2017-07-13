;;; setup-php.el --- PHP Config

;; Copyright (C) 2017 Free Software Foundation, Inc.
;;
;; Author: Evgeny Duzhakov <diabolo@veles>
;; Maintainer: Evgeny Duzhakov <diabolo@veles>
;; Created: 10 Jul 2017
;; Version: 0.01
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
;;   (require 'setup-php)

;;; Code:


;; (require 'php-beautifier)

;; (setq php-beautifier-executable-path "/usr/bin/php_beautifier")
;; (setq php-beautifier-indent-method "tabs")

;; (setq php-beautifier-phpcbf-path "/usr/bin/phpcbf")
;; PHPCS, PSR1, MySource, Squiz, PSR2, Zend and PEAR
;; (setq php-beautifier-phpcbf-standard "PSR2")

(require 'flycheck)
(flycheck-define-checker my-php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0" source)
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in " (file-name) " on line " line line-end))
  :modes (php-mode php+-mode web-mode))

;; ;; (add-hook 'php-mode-hook (lambda () ((flycheck-select-checker my-php)
;; ;;                                      (flycheck-mode nil))))
;; ;; (add-hook 'php-mode-hook (lambda () (flycheck-mode t)))

;; (add-hook 'php-mode-hook (lambda () (local-set-key (kbd "M-;") 'comment-dwim-2)))
;; (add-hook 'php-mode-hook (lambda () (setq comment-multi-line nil ;; maybe
;;                                           comment-start "// "
;;                                           comment-end ""
;;                                           comment-style 'indent
;;                                           comment-use-syntax t)))

(defun my-setup-php ()
  ;; enable web mode
  (web-mode)

  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)
  (add-hook 'web-mode-hook (lambda () (local-set-key (kbd "M-;") 'comment-dwim-2)))
  (add-hook 'web-mode-hook (lambda () (setq comment-multi-line nil ;; maybe
                                            comment-start "// "
                                            comment-end ""
                                            comment-style 'indent
                                            comment-use-syntax t)))
  (setq web-mode-comment-formats
        '(("java"       . "//")
          ("javascript" . "//")
          ("php"        . "//")
          ))
  ;; (setq web-mode-comment-style 2)

  (defun web-mode-comment-php-block (pos)
    (let (beg end)
      (setq beg (web-mode-block-beginning-position pos)
            end (web-(message "message" format-args)ode-block-end-position pos))
      (web-mode-insert-text-at-pos "" (- end 2))
      (web-mode-insert-text-at-pos "//" (+ beg 1 (if (web-mode-looking-at "<\\?php" beg) 5 3)))))

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-words-in-buffer ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))
          ("php" . (ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ac-source-dictionary))))
  ;;
  (flycheck-select-checker my-php)
  (flycheck-mode t))

;; (defun my-setup-php2 ()
;;   (require 'php-cs-fixer)
;;   ;; enable web mode
;;   (php-mode)

;;   (add-hook 'php-mode-hook (lambda () (local-set-key (kbd "M-;") 'comment-dwim-2)))
;;   (add-hook 'php-mode-hook (lambda () (setq comment-start "//"
;; 					    comment-end   "")))
;;   (setq comment-multi-line nil ;; maybe
;;         comment-start "// "
;;         comment-end ""
;;         comment-style 'indent
;;         comment-use-syntax t)

;;   ;; set indentation, can set different indentation level for different code type
;;   (setq php-mode-code-indent-offset 4)
;;   (setq php-mode-css-indent-offset 2)
;;   (setq php-mode-markup-indent-offset 2)
;;   (setq php-mode-ac-sources-alist
;;         '(("php" . (ac-source-words-in-buffer
;;                     ac-source-words-in-same-mode-buffers
;;                     ac-source-dictionary))))
;;   ;;
;;   (flycheck-select-checker my-php)
;;   (flycheck-mode t))

(add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php)) ;

(require 'php-cs-fixer)

;; php documentation
;; (use-package php-eldoc
;;   :ensure t
;;   :init (add-hook 'php-mode-hook 'eldoc-mode))

(use-package company-php
  :ensure t)

(add-hook 'php-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 '((php-extras-company company-dabbrev-code) company-capf company-files))))

;; TODO: use yasnippet to do this
(defun insert-php-doc-comment ()
  "To insert doc comments on php functions."
  (interactive)
  (insert "/**\n * TODO add description. \n * \n * @param \n * @return \n */"))
(define-key global-map [(S-f1)] 'insert-php-doc-comment) ;; shift + F1

;; PHP
;; php-mode
;; TODO: make doc comments to not indent. Make indentation to always use tabs
;; (use-package php-mode
;;   :init(progn
;; 	 ;; Configure per-project phpcs if available
;; 	 (setq-default php-manual-path "~/www/utilidades/docs/php5/php-manual/") ;; php docs local copy
;; 	 ))

;; set psr-2 coding style
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(setq flycheck-phpcs-standard "PSR2")

(add-hook 'php-mode-hook (lambda () (flycheck-mode t)))

;; php-auto-yasnippet
(use-package php-auto-yasnippets)
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

;; php-extras
(use-package php-extras
  :ensure t
  :defer t)



(provide 'setup-php)
;;; setup-php.el ends here
