;;; setup-functions.el --- Useful functions

;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; Author: diabolo <diaevd@gmail.com>
;; Maintainer: diabolo <diaevd@gmail.com>
;; Created: 30 Oct 2016
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
;;   (require 'setup-functions)

;;; Code:

(eval-when-compile
  (require 'cl))

;;------------------------------------------------------------------------
;;
;; Goto functions
;;
;;------------------------------------------------------------------------
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(defun go-to-line-and-column-cond (lc-cond)
  "Allow a specification of LINE:COLUMN or LINE,COLUMN instead of just COLUMN.
Just :COLUMN or ,COLUMN moves to the specified column on the current line.
LINE alone still moves to the beginning of the specified line (like LINE:0 or LINE,0).
By Default I'm bind it to M-g M-l.
The default value of the COLUMN is decrement by -1
because all compilers consider the number of COLUMN from 1 (just for copy-past)"
  (interactive "sLine<[,:]>Column: ")
  (let (line delim column max-lines)
    (setq max-lines (count-lines (point-min) (point-max)))
    (save-match-data
      (string-match "^\\([0-9]*\\)\\([,:]?\\)\\([0-9]*\\)$" lc-cond)
      (setq line (string-to-number (match-string 1 lc-cond)))
      (setq delim (match-string 2 lc-cond))
      (setq column (string-to-number (match-string 3 lc-cond)))
      (if (not (equal delim "")) (if (> column 0) (setq column (1- column))))
      (if (= 0 line) (setq line (line-number-at-pos)))
      (if (> line max-lines) (setq line max-lines))
      (goto-line line)
      (move-to-column column)
      (message "Marker set to line %d column %s" (line-number-at-pos) (current-column))
      )))

(global-set-key (kbd "M-g M-c") 'go-to-column)
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g M-g") 'go-to-line-and-column-cond)

;;
(setq imenu-auto-rescan 't)

(provide 'setup-functions)
;;; setup-functions.el ends here
