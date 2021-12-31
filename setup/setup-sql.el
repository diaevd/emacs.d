;;; setup-sql.el --- SQL mode functions

;; Copyright (C) 2017 Free Software Foundation, Inc.
;;
;; Author: Evgeny Duzhakov <diabolo@veles>
;; Maintainer: Evgeny Duzhakov <diabolo@veles>
;; Created: 21 Aug 2017
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
;;   (require 'setup-sql)

;;; Code:

(eval-when-compile
  (if (version< emacs-version "27.1")
      (require 'cl)
    (require 'cl-lib)))

;; for er/mark-inside-quotes
(straight-use-package 'expand-region)
(use-package sql-indent
  :config
  (defun sql-indent-string ()
    "Indents the string under the cursor as SQL."
    (interactive)
    (save-excursion
      (er/mark-inside-quotes)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (pos (region-beginning))
             (column (progn (goto-char pos) (current-column)))
             (formatted-text (with-temp-buffer
                               (insert text)
                               (delete-trailing-whitespace)
                               (sql-indent-buffer)
                               (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                               (buffer-string))))
        (delete-region (region-beginning) (region-end))
        (goto-char pos)
        (insert formatted-text))))

  (add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-mode-hook 'sqlind-minor-mode)
  (add-hook 'sqlind-minor-mode-hook 'sqlind-setup-style-right)

  (add-to-list 'auto-mode-alist
               '("\\.psql$" . (lambda ()
                                (sql-mode)
                                (sql-highlight-postgres-keywords))))

  (add-hook 'sql-mode-hook
            (lambda ()
            ;; (make-local-variable 'indent-line-function)
            ;; (setq indent-line-function 'ig-indent-sql)
              (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
              (make-local-variable 'company-backends)
              (push '(company-tabnine company-semantic company-files
                                      company-dabbrev-code company-keywords
                                      company-oddmuse company-dabbrev)
                    company-backends)))
  ;;
  ;; Fix SQL indentation
  ;;

  (defun get-previous-indentation ()
    "Get the column of the previous indented line"
    (interactive)
    (save-excursion
      (progn
        (move-beginning-of-line nil)
        (skip-chars-backward "\n \t")
        (back-to-indentation))
      (current-column)))

  (defun get-current-indentation ()
    "Return column at current indentation"
    (interactive)
    (save-excursion
      (progn
        (back-to-indentation)
        (current-column))))

  (defun point-at-current-indentation ()
    "Return point at current indentation"
    (interactive)
    (save-excursion
      (progn
        (move-to-column (get-current-indentation))
        (point))))

  (defun point-at-column-on-line (col)
    "Returns the point at `col` on the current line"
    (interactive)
    (save-excursion
      (progn
        (move-to-column col)
        (point))))

  (defun ig-move-line-to-column (col)
    "Move the line to col; fill with all spaces if moveing forward"
    (interactive "p")
    (let ((point-at-cur-indent (point-at-current-indentation))
          (col-at-cur-indent (get-current-indentation)))
      (cond (
             (= col 0)
             ;; delete to beginning of line or do nothing
             (if (= col-at-cur-indent 0)
                 nil
               (delete-region point-at-cur-indent (point-at-column-on-line 0))))
            (
             (< col col-at-cur-indent)
             ;; delete from our current point BACK to col
             (delete-region (point-at-column-on-line col) point-at-cur-indent))
            (
             (> col col-at-cur-indent)
             ;; delete all text from indent to beginning of line
             (progn
               (delete-region point-at-cur-indent (point-at-column-on-line 0))
               (move-beginning-of-line nil)
               ;; add spaces forward
               (insert (make-string col ?\s)))))))

  (defun ig-indent-sql ()
    "Indent by `tab-width` at most 1 time greater than the previously indented line otherwise go to the beginning of the line indent forward by `tab-width`"
    (let ((previous (get-previous-indentation))
          (current (get-current-indentation)))
      (cond ( ;; exactly at previous line's indentation
             (= previous current)
             (ig-move-line-to-column (+ current tab-width)))

            ( ;; current is greater than previous
             (> current previous)
             ;; exactly at one indentation forward from previous lines indent
             (if (= tab-width (- current previous))
                 ;; move line to beginning
                 (ig-move-line-to-column 0)
               ;; go back to previous indentation level
               (ig-move-line-to-column previous)))

            (t
             (ig-move-line-to-column (+ current tab-width))))))
  )

(provide 'setup-sql)
;;; setup-sql.el ends here
