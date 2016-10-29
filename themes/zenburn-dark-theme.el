;;; zenburn-dark-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2016 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenburn-dark "The Zenburn Dark color theme")

;;; Color Palette

(defvar zenburn-dark-default-colors-alist
  '(("zenburn-dark-fg+1"     . "#FFFFEF")
    ("zenburn-dark-fg"       . "#DCDCCC")
    ("zenburn-dark-fg-1"     . "#656555")
    ("zenburn-dark-bg-2"     . "#000000")
    ("zenburn-dark-bg-1"     . "#2B2B2B")
    ("zenburn-dark-bg-05"    . "#383838")
    ("zenburn-dark-bg"       . "#000000")
    ("zenburn-dark-bg+05"    . "#494949")
    ("zenburn-dark-bg+1"     . "#4F4F4F")
    ("zenburn-dark-bg+2"     . "#5F5F5F")
    ("zenburn-dark-bg+3"     . "#6F6F6F")
    ("zenburn-dark-red+1"    . "#DCA3A3")
    ("zenburn-dark-red"      . "#CC9393")
    ("zenburn-dark-red-1"    . "#BC8383")
    ("zenburn-dark-red-2"    . "#AC7373")
    ("zenburn-dark-red-3"    . "#9C6363")
    ("zenburn-dark-red-4"    . "#8C5353")
    ("zenburn-dark-orange"   . "#DFAF8F")
    ("zenburn-dark-yellow"   . "#F0DFAF")
    ("zenburn-dark-yellow-1" . "#E0CF9F")
    ("zenburn-dark-yellow-2" . "#D0BF8F")
    ("zenburn-dark-green-1"  . "#5F7F5F")
    ("zenburn-dark-green"    . "#7F9F7F")
    ("zenburn-dark-green+1"  . "#8FB28F")
    ("zenburn-dark-green+2"  . "#9FC59F")
    ("zenburn-dark-green+3"  . "#AFD8AF")
    ("zenburn-dark-green+4"  . "#BFEBBF")
    ("zenburn-dark-cyan"     . "#93E0E3")
    ("zenburn-dark-blue+1"   . "#94BFF3")
    ("zenburn-dark-blue"     . "#8CD0D3")
    ("zenburn-dark-blue-1"   . "#7CB8BB")
    ("zenburn-dark-blue-2"   . "#6CA0A3")
    ("zenburn-dark-blue-3"   . "#5C888B")
    ("zenburn-dark-blue-4"   . "#4C7073")
    ("zenburn-dark-blue-5"   . "#366060")
    ("zenburn-dark-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar zenburn-dark-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar zenburn-dark-colors-alist
  (append zenburn-dark-default-colors-alist zenburn-dark-override-colors-alist))

(defmacro zenburn-dark-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-dark-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   zenburn-dark-colors-alist))
     ,@body))

;;; Theme Faces
(zenburn-dark-with-color-variables
  (custom-theme-set-faces
   'zenburn-dark
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenburn-dark-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenburn-dark-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg))))
   `(cursor ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-fg+1))))
   `(escape-glyph ((t (:foreground ,zenburn-dark-yellow :bold t))))
   `(fringe ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg+1))))
   `(header-line ((t (:foreground ,zenburn-dark-yellow
                                  :background ,zenburn-dark-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,zenburn-dark-bg-05))))
   `(success ((t (:foreground ,zenburn-dark-green :weight bold))))
   `(warning ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenburn-dark-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenburn-dark-green))))
   `(compilation-error-face ((t (:foreground ,zenburn-dark-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenburn-dark-fg))))
   `(compilation-info-face ((t (:foreground ,zenburn-dark-blue))))
   `(compilation-info ((t (:foreground ,zenburn-dark-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenburn-dark-green))))
   `(compilation-line-face ((t (:foreground ,zenburn-dark-yellow))))
   `(compilation-line-number ((t (:foreground ,zenburn-dark-yellow))))
   `(compilation-message-face ((t (:foreground ,zenburn-dark-blue))))
   `(compilation-warning-face ((t (:foreground ,zenburn-dark-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenburn-dark-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenburn-dark-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenburn-dark-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenburn-dark-fg))))
   `(grep-error-face ((t (:foreground ,zenburn-dark-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenburn-dark-blue))))
   `(grep-match-face ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(match ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-orange :weight bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenburn-dark-yellow-2 :weight bold :background ,zenburn-dark-bg+2))))
   `(isearch-fail ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-red-4))))
   `(lazy-highlight ((t (:foreground ,zenburn-dark-yellow-2 :weight bold :background ,zenburn-dark-bg-05))))

   `(menu ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenburn-dark-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburn-dark-green+1
                           :background ,zenburn-dark-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenburn-dark-green-1
                      :background ,zenburn-dark-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburn-dark-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenburn-dark-bg+2))))
   `(trailing-whitespace ((t (:background ,zenburn-dark-red))))
   `(vertical-border ((t (:foreground ,zenburn-dark-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenburn-dark-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,zenburn-dark-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-dark-green-1))))
   `(font-lock-constant-face ((t (:foreground ,zenburn-dark-green+4))))
   `(font-lock-doc-face ((t (:foreground ,zenburn-dark-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburn-dark-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburn-dark-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-dark-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,zenburn-dark-red))))
   `(font-lock-type-face ((t (:foreground ,zenburn-dark-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburn-dark-orange))))
   `(font-lock-warning-face ((t (:foreground ,zenburn-dark-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,zenburn-dark-fg))))
   `(newsticker-default-face ((t (:foreground ,zenburn-dark-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,zenburn-dark-green+3))))
   `(newsticker-extra-face ((t (:foreground ,zenburn-dark-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,zenburn-dark-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,zenburn-dark-green))))
   `(newsticker-new-item-face ((t (:foreground ,zenburn-dark-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,zenburn-dark-red))))
   `(newsticker-old-item-face ((t (:foreground ,zenburn-dark-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,zenburn-dark-fg))))
   `(newsticker-treeview-face ((t (:foreground ,zenburn-dark-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,zenburn-dark-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenburn-dark-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,zenburn-dark-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenburn-dark-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,zenburn-dark-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,zenburn-dark-fg-1 :background ,zenburn-dark-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,zenburn-dark-fg-1 :background ,zenburn-dark-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,zenburn-dark-green+1))))
   `(android-mode-error-face ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,zenburn-dark-fg))))
   `(android-mode-verbose-face ((t (:foreground ,zenburn-dark-green))))
   `(android-mode-warning-face ((t (:foreground ,zenburn-dark-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,zenburn-dark-cyan :weight bold))))
   `(anzu-match-1 ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-green))))
   `(anzu-match-2 ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-orange))))
   `(anzu-match-3 ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenburn-dark-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenburn-dark-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenburn-dark-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenburn-dark-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenburn-dark-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,zenburn-dark-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,zenburn-dark-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenburn-dark-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenburn-dark-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenburn-dark-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,zenburn-dark-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,zenburn-dark-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,zenburn-dark-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,zenburn-dark-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,zenburn-dark-bg+3 :foreground ,zenburn-dark-bg-2))))
   `(ac-selection-face ((t (:background ,zenburn-dark-blue-4 :foreground ,zenburn-dark-fg))))
   `(popup-tip-face ((t (:background ,zenburn-dark-yellow-2 :foreground ,zenburn-dark-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,zenburn-dark-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,zenburn-dark-bg-1))))
   `(popup-isearch-match ((t (:background ,zenburn-dark-bg :foreground ,zenburn-dark-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,zenburn-dark-fg-1 :background ,zenburn-dark-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,zenburn-dark-green+3 :background ,zenburn-dark-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,zenburn-dark-red+1 :background ,zenburn-dark-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenburn-dark-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenburn-dark-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenburn-dark-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenburn-dark-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenburn-dark-bg+2))))
   `(company-preview ((t (:background ,zenburn-dark-green+2))))
   `(company-preview-common ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,zenburn-dark-yellow-1 :foreground ,zenburn-dark-bg+1))))
   `(bm-fringe-face ((t (:background ,zenburn-dark-yellow-1 :foreground ,zenburn-dark-bg+1))))
   `(bm-fringe-persistent-face ((t (:background ,zenburn-dark-green-1 :foreground ,zenburn-dark-bg+1))))
   `(bm-persistent-face ((t (:background ,zenburn-dark-green-1 :foreground ,zenburn-dark-bg+1))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,zenburn-dark-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenburn-dark-green+1))))
   `(cider-deprecated-face ((t (:background ,zenburn-dark-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,zenburn-dark-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,zenburn-dark-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,zenburn-dark-red-4))))
   `(cider-test-error-face ((t (:background ,zenburn-dark-magenta))))
   `(cider-test-success-face ((t (:background ,zenburn-dark-green-1))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,zenburn-dark-cyan))))
   `(circe-my-message-face ((t (:foreground ,zenburn-dark-fg))))
   `(circe-fool-face ((t (:foreground ,zenburn-dark-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,zenburn-dark-fg))))
   `(circe-server-face ((t (:foreground ,zenburn-dark-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,zenburn-dark-fg)))
   `(context-coloring-level-1-face ((t :foreground ,zenburn-dark-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,zenburn-dark-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,zenburn-dark-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,zenburn-dark-orange)))
   `(context-coloring-level-5-face ((t :foreground ,zenburn-dark-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,zenburn-dark-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,zenburn-dark-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,zenburn-dark-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,zenburn-dark-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,zenburn-dark-blue :foreground ,zenburn-dark-bg+1))))
   `(ctbl:face-continue-bar ((t (:background ,zenburn-dark-bg-05 :foreground ,zenburn-dark-bg+1))))
   `(ctbl:face-row-select ((t (:background ,zenburn-dark-cyan :foreground ,zenburn-dark-bg+1))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,zenburn-dark-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenburn-dark-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,zenburn-dark-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,zenburn-dark-green+4))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,zenburn-dark-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,zenburn-dark-red))))
   `(diff-header ((,class (:background ,zenburn-dark-bg+2))
                  (t (:background ,zenburn-dark-fg :foreground ,zenburn-dark-bg))))
   `(diff-file-header
     ((,class (:background ,zenburn-dark-bg+2 :foreground ,zenburn-dark-fg :bold t))
      (t (:background ,zenburn-dark-fg :foreground ,zenburn-dark-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenburn-dark-blue :background ,zenburn-dark-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenburn-dark-red+1 :background ,zenburn-dark-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenburn-dark-green+1 :background ,zenburn-dark-green-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,zenburn-dark-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,zenburn-dark-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,zenburn-dark-orange))))
   `(diredp-date-time ((t (:foreground ,zenburn-dark-magenta))))
   `(diredp-deletion ((t (:foreground ,zenburn-dark-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,zenburn-dark-red))))
   `(diredp-dir-heading ((t (:foreground ,zenburn-dark-blue :background ,zenburn-dark-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,zenburn-dark-cyan))))
   `(diredp-exec-priv ((t (:foreground ,zenburn-dark-red))))
   `(diredp-executable-tag ((t (:foreground ,zenburn-dark-green+1))))
   `(diredp-file-name ((t (:foreground ,zenburn-dark-blue))))
   `(diredp-file-suffix ((t (:foreground ,zenburn-dark-green))))
   `(diredp-flag-mark ((t (:foreground ,zenburn-dark-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,zenburn-dark-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,zenburn-dark-red))))
   `(diredp-link-priv ((t (:foreground ,zenburn-dark-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,zenburn-dark-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,zenburn-dark-orange))))
   `(diredp-no-priv ((t (:foreground ,zenburn-dark-fg))))
   `(diredp-number ((t (:foreground ,zenburn-dark-green+1))))
   `(diredp-other-priv ((t (:foreground ,zenburn-dark-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,zenburn-dark-red-1))))
   `(diredp-read-priv ((t (:foreground ,zenburn-dark-green-1))))
   `(diredp-symlink ((t (:foreground ,zenburn-dark-yellow))))
   `(diredp-write-priv ((t (:foreground ,zenburn-dark-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(dired-async-message ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,zenburn-dark-yellow))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-blue-5))))
   `(ediff-even-diff-A ((t (:background ,zenburn-dark-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,zenburn-dark-bg+1))))
   `(ediff-even-diff-B ((t (:background ,zenburn-dark-bg+1))))
   `(ediff-even-diff-C ((t (:background ,zenburn-dark-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,zenburn-dark-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,zenburn-dark-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,zenburn-dark-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,zenburn-dark-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,zenburn-dark-fg))))
   `(egg-help-header-1 ((t (:foreground ,zenburn-dark-yellow))))
   `(egg-help-header-2 ((t (:foreground ,zenburn-dark-green+3))))
   `(egg-branch ((t (:foreground ,zenburn-dark-yellow))))
   `(egg-branch-mono ((t (:foreground ,zenburn-dark-yellow))))
   `(egg-term ((t (:foreground ,zenburn-dark-yellow))))
   `(egg-diff-add ((t (:foreground ,zenburn-dark-green+4))))
   `(egg-diff-del ((t (:foreground ,zenburn-dark-red+1))))
   `(egg-diff-file-header ((t (:foreground ,zenburn-dark-yellow-2))))
   `(egg-section-title ((t (:foreground ,zenburn-dark-yellow))))
   `(egg-stash-mono ((t (:foreground ,zenburn-dark-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,zenburn-dark-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,zenburn-dark-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,zenburn-dark-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,zenburn-dark-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,zenburn-dark-green))))
   `(elfeed-search-feed-face ((t (:foreground ,zenburn-dark-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,zenburn-dark-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,zenburn-dark-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,zenburn-dark-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,zenburn-dark-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))
   `(w3m-lnum-match ((t (:background ,zenburn-dark-bg-1
                                     :foreground ,zenburn-dark-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenburn-dark-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,zenburn-dark-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,zenburn-dark-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,zenburn-dark-yellow))))
   `(erc-keyword-face ((t (:foreground ,zenburn-dark-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,zenburn-dark-green))))
   `(erc-pal-face ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,zenburn-dark-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenburn-dark-green+4 :background ,zenburn-dark-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,zenburn-dark-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,zenburn-dark-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,zenburn-dark-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,zenburn-dark-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,zenburn-dark-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,zenburn-dark-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-red-1) :inherit unspecified))
      (t (:foreground ,zenburn-dark-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-yellow) :inherit unspecified))
      (t (:foreground ,zenburn-dark-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-cyan) :inherit unspecified))
      (t (:foreground ,zenburn-dark-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,zenburn-dark-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,zenburn-dark-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-dark-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-dark-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-dark-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-orange) :inherit unspecified))
      (t (:foreground ,zenburn-dark-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-red) :inherit unspecified))
      (t (:foreground ,zenburn-dark-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,zenburn-dark-fg))))
   `(ack-file ((t (:foreground ,zenburn-dark-blue))))
   `(ack-line ((t (:foreground ,zenburn-dark-yellow))))
   `(ack-match ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburn-dark-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburn-dark-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburn-dark-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,zenburn-dark-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,zenburn-dark-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,zenburn-dark-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,zenburn-dark-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,zenburn-dark-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,zenburn-dark-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, zenburn-dark-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,zenburn-dark-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,zenburn-dark-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,zenburn-dark-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,zenburn-dark-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,zenburn-dark-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,zenburn-dark-blue))))
   `(gnus-summary-high-read ((t (:foreground ,zenburn-dark-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,zenburn-dark-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,zenburn-dark-blue))))
   `(gnus-summary-low-read ((t (:foreground ,zenburn-dark-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,zenburn-dark-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-dark-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburn-dark-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburn-dark-fg))))
   `(gnus-summary-selected ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,zenburn-dark-blue))))
   `(gnus-cite-10 ((t (:foreground ,zenburn-dark-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,zenburn-dark-yellow))))
   `(gnus-cite-2 ((t (:foreground ,zenburn-dark-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,zenburn-dark-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,zenburn-dark-green+2))))
   `(gnus-cite-5 ((t (:foreground ,zenburn-dark-green+1))))
   `(gnus-cite-6 ((t (:foreground ,zenburn-dark-green))))
   `(gnus-cite-7 ((t (:foreground ,zenburn-dark-red))))
   `(gnus-cite-8 ((t (:foreground ,zenburn-dark-red-1))))
   `(gnus-cite-9 ((t (:foreground ,zenburn-dark-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,zenburn-dark-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,zenburn-dark-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,zenburn-dark-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,zenburn-dark-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,zenburn-dark-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,zenburn-dark-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,zenburn-dark-bg+2))))
   `(gnus-signature ((t (:foreground ,zenburn-dark-yellow))))
   `(gnus-x ((t (:background ,zenburn-dark-fg :foreground ,zenburn-dark-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,zenburn-dark-blue))))
   `(guide-key/key-face ((t (:foreground ,zenburn-dark-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,zenburn-dark-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,zenburn-dark-green
                      :background ,zenburn-dark-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,zenburn-dark-yellow
                      :background ,zenburn-dark-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,zenburn-dark-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenburn-dark-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenburn-dark-green+4 :background ,zenburn-dark-bg-1))))
   `(helm-separator ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenburn-dark-magenta :background ,zenburn-dark-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenburn-dark-magenta :background ,zenburn-dark-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg))))
   `(helm-buffer-process ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg))))
   `(helm-buffer-size ((t (:foreground ,zenburn-dark-fg-1 :background ,zenburn-dark-bg))))
   `(helm-ff-directory ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg))))
   `(helm-grep-file ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg))))
   `(helm-grep-finish ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenburn-dark-fg-1 :background ,zenburn-dark-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg))))
   `(helm-match ((t (:foreground ,zenburn-dark-orange :background ,zenburn-dark-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenburn-dark-fg-1 :background ,zenburn-dark-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-dark-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburn-dark-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburn-dark-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,zenburn-dark-red-1 :background ,zenburn-dark-bg))))
   `(hydra-face-amaranth ((t (:foreground ,zenburn-dark-red-3 :background ,zenburn-dark-bg))))
   `(hydra-face-blue ((t (:foreground ,zenburn-dark-blue :background ,zenburn-dark-bg))))
   `(hydra-face-pink ((t (:foreground ,zenburn-dark-magenta :background ,zenburn-dark-bg))))
   `(hydra-face-teal ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,zenburn-dark-green :background ,zenburn-dark-bg))))
   `(ivy-match-required-face ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-bg))))
   `(ivy-remote ((t (:foreground ,zenburn-dark-blue :background ,zenburn-dark-bg))))
   `(ivy-subdir ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-bg))))
   `(ivy-current-match ((t (:foreground ,zenburn-dark-yellow :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,zenburn-dark-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,zenburn-dark-green-1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,zenburn-dark-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,zenburn-dark-green+1))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,zenburn-dark-yellow))))
   `(ido-indicator ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,zenburn-dark-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,zenburn-dark-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,zenburn-dark-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,zenburn-dark-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,zenburn-dark-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,zenburn-dark-orange))))
   `(jabber-roster-user-error ((t (:foreground ,zenburn-dark-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,zenburn-dark-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,zenburn-dark-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,zenburn-dark-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,zenburn-dark-green+3))))
   `(jabber-activity-face((t (:foreground ,zenburn-dark-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,zenburn-dark-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,zenburn-dark-orange))))
   `(js2-error ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,zenburn-dark-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,zenburn-dark-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,zenburn-dark-green+3))))
   `(js2-function-param ((t (:foreground, zenburn-dark-orange))))
   `(js2-external-variable ((t (:foreground ,zenburn-dark-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,zenburn-dark-green-1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenburn-dark-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenburn-dark-red-1))))
   `(js2-object-property ((t (:foreground ,zenburn-dark-blue+1))))
   `(js2-magic-paren ((t (:foreground ,zenburn-dark-blue-5))))
   `(js2-private-function-call ((t (:foreground ,zenburn-dark-cyan))))
   `(js2-function-call ((t (:foreground ,zenburn-dark-cyan))))
   `(js2-private-member ((t (:foreground ,zenburn-dark-blue-1))))
   `(js2-keywords ((t (:foreground ,zenburn-dark-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenburn-dark-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,zenburn-dark-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,zenburn-dark-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,zenburn-dark-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,zenburn-dark-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,zenburn-dark-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenburn-dark-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenburn-dark-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,zenburn-dark-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,zenburn-dark-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,zenburn-dark-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,zenburn-dark-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenburn-dark-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenburn-dark-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenburn-dark-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,zenburn-dark-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,zenburn-dark-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,zenburn-dark-bg+1 :background ,zenburn-dark-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenburn-dark-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenburn-dark-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenburn-dark-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,zenburn-dark-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,zenburn-dark-blue-1))))
   `(lui-hilight-face ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,zenburn-dark-green+2 :background ,zenburn-dark-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,zenburn-dark-red+1 :background ,zenburn-dark-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,zenburn-dark-blue+1 :background ,zenburn-dark-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,zenburn-dark-magenta :background ,zenburn-dark-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,zenburn-dark-yellow :background ,zenburn-dark-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,zenburn-dark-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenburn-dark-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenburn-dark-bg+05
                                                        :foreground ,zenburn-dark-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,zenburn-dark-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburn-dark-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenburn-dark-bg+2
                                                        :foreground ,zenburn-dark-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenburn-dark-orange
                                                        :foreground ,zenburn-dark-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenburn-dark-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,zenburn-dark-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,zenburn-dark-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenburn-dark-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenburn-dark-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenburn-dark-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenburn-dark-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenburn-dark-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenburn-dark-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenburn-dark-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenburn-dark-orange))))
   `(magit-log-date      ((t (:foreground ,zenburn-dark-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenburn-dark-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenburn-dark-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenburn-dark-green))))
   `(magit-sequence-part ((t (:foreground ,zenburn-dark-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenburn-dark-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenburn-dark-red))))
   `(magit-sequence-done ((t (:foreground ,zenburn-dark-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenburn-dark-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenburn-dark-green))))
   `(magit-bisect-skip ((t (:foreground ,zenburn-dark-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenburn-dark-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-blue-2))))
   `(magit-blame-name    ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-orange))))
   `(magit-blame-date    ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-orange))))
   `(magit-blame-summary ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenburn-dark-bg+3))))
   `(magit-hash           ((t (:foreground ,zenburn-dark-bg+3))))
   `(magit-tag            ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenburn-dark-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenburn-dark-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenburn-dark-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenburn-dark-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenburn-dark-bg+2 :foreground ,zenburn-dark-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenburn-dark-bg+2 :foreground ,zenburn-dark-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenburn-dark-bg+2 :foreground ,zenburn-dark-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenburn-dark-green))))
   `(magit-signature-bad       ((t (:foreground ,zenburn-dark-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenburn-dark-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenburn-dark-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenburn-dark-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenburn-dark-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenburn-dark-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenburn-dark-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenburn-dark-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenburn-dark-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenburn-dark-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburn-dark-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenburn-dark-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenburn-dark-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenburn-dark-green+1))))
   `(message-header-other ((t (:foreground ,zenburn-dark-green))))
   `(message-header-to ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenburn-dark-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenburn-dark-green))))
   `(message-mml ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,zenburn-dark-orange))))
   `(mew-face-header-from ((t (:foreground ,zenburn-dark-yellow))))
   `(mew-face-header-date ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-header-to ((t (:foreground ,zenburn-dark-red))))
   `(mew-face-header-key ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-header-private ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-header-important ((t (:foreground ,zenburn-dark-blue))))
   `(mew-face-header-marginal ((t (:foreground ,zenburn-dark-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,zenburn-dark-red))))
   `(mew-face-header-xmew ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,zenburn-dark-red))))
   `(mew-face-body-url ((t (:foreground ,zenburn-dark-orange))))
   `(mew-face-body-comment ((t (:foreground ,zenburn-dark-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-body-cite2 ((t (:foreground ,zenburn-dark-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,zenburn-dark-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,zenburn-dark-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,zenburn-dark-red))))
   `(mew-face-mark-review ((t (:foreground ,zenburn-dark-blue))))
   `(mew-face-mark-escape ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-mark-delete ((t (:foreground ,zenburn-dark-red))))
   `(mew-face-mark-unlink ((t (:foreground ,zenburn-dark-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-mark-unread ((t (:foreground ,zenburn-dark-red-2))))
   `(mew-face-eof-message ((t (:foreground ,zenburn-dark-green))))
   `(mew-face-eof-part ((t (:foreground ,zenburn-dark-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,zenburn-dark-cyan :background ,zenburn-dark-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,zenburn-dark-blue))))
   `(mingus-pausing-face ((t (:foreground ,zenburn-dark-magenta))))
   `(mingus-playing-face ((t (:foreground ,zenburn-dark-cyan))))
   `(mingus-playlist-face ((t (:foreground ,zenburn-dark-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,zenburn-dark-yellow))))
   `(mingus-stopped-face ((t (:foreground ,zenburn-dark-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,zenburn-dark-yellow))))
   `(nav-face-button-num ((t (:foreground ,zenburn-dark-cyan))))
   `(nav-face-dir ((t (:foreground ,zenburn-dark-green))))
   `(nav-face-hdir ((t (:foreground ,zenburn-dark-red))))
   `(nav-face-file ((t (:foreground ,zenburn-dark-fg))))
   `(nav-face-hfile ((t (:foreground ,zenburn-dark-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,zenburn-dark-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,zenburn-dark-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,zenburn-dark-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,zenburn-dark-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,zenburn-dark-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,zenburn-dark-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,zenburn-dark-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,zenburn-dark-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,zenburn-dark-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,zenburn-dark-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,zenburn-dark-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,zenburn-dark-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,zenburn-dark-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,zenburn-dark-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,zenburn-dark-fg))))
   `(neo-root-dir-face ((t (:foreground ,zenburn-dark-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,zenburn-dark-blue))))
   `(neo-file-link-face ((t (:foreground ,zenburn-dark-fg))))
   `(neo-expand-btn-face ((t (:foreground ,zenburn-dark-blue))))
   `(neo-vc-default-face ((t (:foreground ,zenburn-dark-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,zenburn-dark-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,zenburn-dark-fg))))
   `(neo-vc-edited-face ((t (:foreground ,zenburn-dark-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,zenburn-dark-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenburn-dark-red :background ,zenburn-dark-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,zenburn-dark-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,zenburn-dark-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,zenburn-dark-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,zenburn-dark-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,zenburn-dark-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,zenburn-dark-fg :weight bold))))
   `(org-checkbox ((t (:background ,zenburn-dark-bg+2 :foreground ,zenburn-dark-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,zenburn-dark-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,zenburn-dark-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,zenburn-dark-green+3))))
   `(org-formula ((t (:foreground ,zenburn-dark-yellow-2))))
   `(org-headline-done ((t (:foreground ,zenburn-dark-green+3))))
   `(org-hide ((t (:foreground ,zenburn-dark-bg-1))))
   `(org-level-1 ((t (:foreground ,zenburn-dark-orange))))
   `(org-level-2 ((t (:foreground ,zenburn-dark-green+4))))
   `(org-level-3 ((t (:foreground ,zenburn-dark-blue-1))))
   `(org-level-4 ((t (:foreground ,zenburn-dark-yellow-2))))
   `(org-level-5 ((t (:foreground ,zenburn-dark-cyan))))
   `(org-level-6 ((t (:foreground ,zenburn-dark-green+2))))
   `(org-level-7 ((t (:foreground ,zenburn-dark-red-4))))
   `(org-level-8 ((t (:foreground ,zenburn-dark-blue-4))))
   `(org-link ((t (:foreground ,zenburn-dark-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,zenburn-dark-green+4))))
   `(org-scheduled-previously ((t (:foreground ,zenburn-dark-red))))
   `(org-scheduled-today ((t (:foreground ,zenburn-dark-blue+1))))
   `(org-sexp-date ((t (:foreground ,zenburn-dark-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,zenburn-dark-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,zenburn-dark-orange))))
   `(org-todo ((t (:bold t :foreground ,zenburn-dark-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,zenburn-dark-red :weight bold :underline nil))))
   `(org-column ((t (:background ,zenburn-dark-bg-1))))
   `(org-column-title ((t (:background ,zenburn-dark-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-red-1))))
   `(org-ellipsis ((t (:foreground ,zenburn-dark-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,zenburn-dark-cyan :underline t))))
   `(org-document-title ((t (:foreground ,zenburn-dark-blue))))
   `(org-document-info ((t (:foreground ,zenburn-dark-blue))))
   `(org-habit-ready-face ((t :background ,zenburn-dark-green)))
   `(org-habit-alert-face ((t :background ,zenburn-dark-yellow-1 :foreground ,zenburn-dark-bg)))
   `(org-habit-clear-face ((t :background ,zenburn-dark-blue-3)))
   `(org-habit-overdue-face ((t :background ,zenburn-dark-red-3)))
   `(org-habit-clear-future-face ((t :background ,zenburn-dark-blue-4)))
   `(org-habit-ready-future-face ((t :background ,zenburn-dark-green-1)))
   `(org-habit-alert-future-face ((t :background ,zenburn-dark-yellow-2 :foreground ,zenburn-dark-bg)))
   `(org-habit-overdue-future-face ((t :background ,zenburn-dark-red-4)))
;;;;; outline
   `(outline-1 ((t (:foreground ,zenburn-dark-orange))))
   `(outline-2 ((t (:foreground ,zenburn-dark-green+4))))
   `(outline-3 ((t (:foreground ,zenburn-dark-blue-1))))
   `(outline-4 ((t (:foreground ,zenburn-dark-yellow-2))))
   `(outline-5 ((t (:foreground ,zenburn-dark-cyan))))
   `(outline-6 ((t (:foreground ,zenburn-dark-green+2))))
   `(outline-7 ((t (:foreground ,zenburn-dark-red-4))))
   `(outline-8 ((t (:foreground ,zenburn-dark-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,zenburn-dark-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,zenburn-dark-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,zenburn-dark-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,zenburn-dark-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,zenburn-dark-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-orange))))
   `(proof-error-face ((t (:foreground ,zenburn-dark-fg :background ,zenburn-dark-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-orange))))
   `(proof-locked-face ((t (:background ,zenburn-dark-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-orange))))
   `(proof-queue-face ((t (:background ,zenburn-dark-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,zenburn-dark-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-dark-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-dark-bg))))
   `(proof-warning-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-dark-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-dark-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-dark-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-dark-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-dark-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-dark-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-dark-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-dark-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-dark-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-dark-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-dark-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-dark-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,zenburn-dark-blue))))
   `(rcirc-other-nick ((t (:foreground ,zenburn-dark-orange))))
   `(rcirc-bright-nick ((t (:foreground ,zenburn-dark-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,zenburn-dark-blue-2))))
   `(rcirc-server ((t (:foreground ,zenburn-dark-green))))
   `(rcirc-server-prefix ((t (:foreground ,zenburn-dark-green+1))))
   `(rcirc-timestamp ((t (:foreground ,zenburn-dark-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,zenburn-dark-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,zenburn-dark-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,zenburn-dark-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,zenburn-dark-green))))
   `(rpm-spec-doc-face ((t (:foreground ,zenburn-dark-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,zenburn-dark-red))))
   `(rpm-spec-macro-face ((t (:foreground ,zenburn-dark-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenburn-dark-red))))
   `(rpm-spec-package-face ((t (:foreground ,zenburn-dark-red))))
   `(rpm-spec-section-face ((t (:foreground ,zenburn-dark-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,zenburn-dark-blue))))
   `(rpm-spec-var-face ((t (:foreground ,zenburn-dark-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,zenburn-dark-orange))))
   `(rst-level-2-face ((t (:foreground ,zenburn-dark-green+1))))
   `(rst-level-3-face ((t (:foreground ,zenburn-dark-blue-1))))
   `(rst-level-4-face ((t (:foreground ,zenburn-dark-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,zenburn-dark-cyan))))
   `(rst-level-6-face ((t (:foreground ,zenburn-dark-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,zenburn-dark-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,zenburn-dark-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenburn-dark-red+1 :background ,zenburn-dark-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,zenburn-dark-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Zenburn for sml
   `(sml/global ((,class (:foreground ,zenburn-dark-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,zenburn-dark-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,zenburn-dark-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,zenburn-dark-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,zenburn-dark-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,zenburn-dark-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,zenburn-dark-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,zenburn-dark-orange))))
   `(sml/git ((,class (:foreground ,zenburn-dark-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,zenburn-dark-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,zenburn-dark-red-2))))
   `(sml/outside-modified ((,class (:foreground ,zenburn-dark-orange))))
   `(sml/modified ((,class (:foreground ,zenburn-dark-red))))
   `(sml/vc-edited ((,class (:foreground ,zenburn-dark-green+2))))
   `(sml/charging ((,class (:foreground ,zenburn-dark-green+4))))
   `(sml/discharging ((,class (:foreground ,zenburn-dark-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,zenburn-dark-red+1 :background ,zenburn-dark-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,zenburn-dark-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,zenburn-dark-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,zenburn-dark-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-red)))
      (t
       (:underline ,zenburn-dark-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-orange)))
      (t
       (:underline ,zenburn-dark-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-yellow)))
      (t
       (:underline ,zenburn-dark-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-dark-green)))
      (t
       (:underline ,zenburn-dark-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,zenburn-dark-green+2))))
   `(speedbar-directory-face ((t (:foreground ,zenburn-dark-cyan))))
   `(speedbar-file-face ((t (:foreground ,zenburn-dark-fg))))
   `(speedbar-highlight-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-green+2))))
   `(speedbar-selected-face ((t (:foreground ,zenburn-dark-red))))
   `(speedbar-separator-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,zenburn-dark-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,zenburn-dark-fg
                                    :background ,zenburn-dark-bg))))
   `(tabbar-selected ((t (:foreground ,zenburn-dark-fg
                                      :background ,zenburn-dark-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,zenburn-dark-fg
                                        :background ,zenburn-dark-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenburn-dark-bg
                                       :background ,zenburn-dark-bg-1))))
   `(term-color-red ((t (:foreground ,zenburn-dark-red-2
                                     :background ,zenburn-dark-red-4))))
   `(term-color-green ((t (:foreground ,zenburn-dark-green
                                       :background ,zenburn-dark-green+2))))
   `(term-color-yellow ((t (:foreground ,zenburn-dark-orange
                                        :background ,zenburn-dark-yellow))))
   `(term-color-blue ((t (:foreground ,zenburn-dark-blue-1
                                      :background ,zenburn-dark-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenburn-dark-magenta
                                         :background ,zenburn-dark-red))))
   `(term-color-cyan ((t (:foreground ,zenburn-dark-cyan
                                      :background ,zenburn-dark-blue))))
   `(term-color-white ((t (:foreground ,zenburn-dark-fg
                                       :background ,zenburn-dark-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburn-dark-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,zenburn-dark-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,zenburn-dark-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,zenburn-dark-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburn-dark-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,zenburn-dark-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenburn-dark-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenburn-dark-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburn-dark-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenburn-dark-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenburn-dark-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenburn-dark-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenburn-dark-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenburn-dark-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenburn-dark-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenburn-dark-bg+1 :foreground ,zenburn-dark-bg+1))))
   `(whitespace-hspace ((t (:background ,zenburn-dark-bg+1 :foreground ,zenburn-dark-bg+1))))
   `(whitespace-tab ((t (:background ,zenburn-dark-red-1))))
   `(whitespace-newline ((t (:foreground ,zenburn-dark-bg+1))))
   `(whitespace-trailing ((t (:background ,zenburn-dark-red))))
   `(whitespace-line ((t (:background ,zenburn-dark-bg :foreground ,zenburn-dark-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenburn-dark-orange :foreground ,zenburn-dark-orange))))
   `(whitespace-indentation ((t (:background ,zenburn-dark-yellow :foreground ,zenburn-dark-red))))
   `(whitespace-empty ((t (:background ,zenburn-dark-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenburn-dark-yellow :foreground ,zenburn-dark-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,zenburn-dark-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,zenburn-dark-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenburn-dark-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburn-dark-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,zenburn-dark-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenburn-dark-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,zenburn-dark-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburn-dark-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburn-dark-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenburn-dark-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburn-dark-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenburn-dark-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,zenburn-dark-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenburn-dark-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,zenburn-dark-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenburn-dark-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,zenburn-dark-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenburn-dark-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,zenburn-dark-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenburn-dark-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenburn-dark-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,zenburn-dark-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburn-dark-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,zenburn-dark-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenburn-dark-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,zenburn-dark-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,zenburn-dark-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,zenburn-dark-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,zenburn-dark-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,zenburn-dark-bg :background ,zenburn-dark-blue+1))))
   `(cscope-separator-face ((t (:foreground ,zenburn-dark-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,zenburn-dark-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,zenburn-dark-bg-1 :foreground ,zenburn-dark-bg-1))))
   ))

;;; Theme Variables
(zenburn-dark-with-color-variables
  (custom-theme-set-variables
   'zenburn-dark
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburn-dark-bg ,zenburn-dark-red ,zenburn-dark-green ,zenburn-dark-yellow
                                          ,zenburn-dark-blue ,zenburn-dark-magenta ,zenburn-dark-cyan ,zenburn-dark-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburn-dark-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenburn-dark-red ,zenburn-dark-orange ,zenburn-dark-yellow ,zenburn-dark-green ,zenburn-dark-green+4
                    ,zenburn-dark-cyan ,zenburn-dark-blue+1 ,zenburn-dark-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenburn-dark-fg . ,zenburn-dark-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburn-dark-red-1)
       ( 40. . ,zenburn-dark-red)
       ( 60. . ,zenburn-dark-orange)
       ( 80. . ,zenburn-dark-yellow-2)
       (100. . ,zenburn-dark-yellow-1)
       (120. . ,zenburn-dark-yellow)
       (140. . ,zenburn-dark-green-1)
       (160. . ,zenburn-dark-green)
       (180. . ,zenburn-dark-green+1)
       (200. . ,zenburn-dark-green+2)
       (220. . ,zenburn-dark-green+3)
       (240. . ,zenburn-dark-green+4)
       (260. . ,zenburn-dark-cyan)
       (280. . ,zenburn-dark-blue-2)
       (300. . ,zenburn-dark-blue-1)
       (320. . ,zenburn-dark-blue)
       (340. . ,zenburn-dark-blue+1)
       (360. . ,zenburn-dark-magenta)))
   `(vc-annotate-very-old-color ,zenburn-dark-magenta)
   `(vc-annotate-background ,zenburn-dark-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar zenburn-dark-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburn color names.
In buffers visiting library `zenburn-dark-theme.el' the zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar zenburn-dark-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after zenburn activate)
;;   "Maybe also add font-lock keywords for zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or zenburn-dark-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "zenburn-dark-theme.el")))
;;     (unless zenburn-dark-colors-font-lock-keywords
;;       (setq zenburn-dark-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car zenburn-dark-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc zenburn-dark-colors-alist))))))
;;     (font-lock-add-keywords nil zenburn-dark-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after zenburn activate)
;;   "Also remove font-lock keywords for zenburn colors."
;;   (font-lock-remove-keywords nil zenburn-dark-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenburn-dark)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenburn-dark-theme.el ends here
