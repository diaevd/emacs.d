;; ------------------------------------------------------------------------
;; Perl Autocomplete
;; perl6
;;
;; https://github.com/hinrik/perl6-mode
;; Use M-x customize-group RET perl6 to customize Perl 6 Mode.
;;------------------------------------------------------------------------
(use-package perl6-mode
	     :ensure t
	     :defer t)

;;------------------------------------------------------------------------
;;
;; perl5
;;
;;------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/pde/lisp")
(load "pde-load")

;;;; Opening and Creating Files
;; Commands
;; C-x C-f ido-find-file
;; C-x C-b ido-switch-buffer

;;;; Editing
;; Commands
;; C-M-\ indent-region
;; C-M-= pde-indent-dwim
;; C-; comment-dwim
;; M-/ dabbrev-expand
;; M-; hippie-expand
;; C-c f comint-dynamic-complete
;; M-' just-one-space
;; M-\ delete-horizontal-space
;; C-M-a beginning-of-defun
;; C-M-e end-of-defun
;; C-M-f forward-sexp
;; C-M-b backward-sexp
;; C-M-u backward-up-list
;; M-{ backward-paragraph
;; M-} forward-paragraph
;; M-h mark-paragraph
;; C-M-h mark-defun
;; C-x h mark-whole-buffer
;; M-SPC not available, window manager take it away
(global-set-key (kbd "M-'") 'just-one-space)
(global-set-key (kbd "C-M-=") 'pde-indent-dwim)
;; nearest key to dabbrev-expand
(global-set-key (kbd "C-;") 'hippie-expand)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "C-c f") 'comint-dynamic-complete)

(setq hippie-expand-try-functions-list
      '(try-expand-line
	try-expand-dabbrev
	try-expand-line-all-buffers
	try-expand-list
	try-expand-list-all-buffers
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name
	try-complete-file-name-partially
	try-complete-lisp-symbol
	try-complete-lisp-symbol-partially
	try-expand-whole-kill))
(autoload 'comint-dynamic-complete "comint" "Complete for file name" t)
(setq comint-completion-addsuffix '("/" . ""))
(setq-default indent-tabs-mode nil)

(defalias 'perl-mode 'cperl-mode)
(defun pde-perl-mode-hook ()
  (abbrev-mode t)
  (ido-mode 1)
  (require 'template-simple)
  (when (and buffer-file-name
	     (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
    (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil)
  (add-to-list 'cperl-style-alist
	       '("PDE"
		 (cperl-auto-newline                         . nil)
		 (cperl-brace-offset                         . 0)
		 (cperl-close-paren-offset                   . -8)
		 (cperl-continued-brace-offset               . 0)
		 (cperl-continued-statement-offset           . 0)
		 (cperl-extra-newline-before-brace           . nil)
		 (cperl-extra-newline-before-brace-multiline . nil)
		 (cperl-indent-level                         . 8)
		 (cperl-indent-parens-as-block               . t)
		 (cperl-indent-tabs-mode                     . t)
		 (cperl-label-offset                         . -8)
		 (cperl-merge-trailing-else                  . t)
		 (cperl-tab-always-indent                    . t)))
  (cperl-set-style "PDE"))

;;;; Syntax Checking and Running
;; Commands
;; C-c s compile-dwim-compile
;; C-c r compile-dwim-run
;; compile
;; flymake-mode
;; executable-set-magic

(global-set-key (kbd "C-c s") 'compile-dwim-compile)
(global-set-key (kbd "C-c r") 'compile-dwim-run)
(setq compilation-buffer-name-function 'pde-compilation-buffer-name)
(autoload 'compile-dwim-run "compile-dwim" "Build and run" t)
(autoload 'compile-dwim-compile "compile-dwim" "Compile or check syntax" t)
(autoload 'executable-chmod "executable"
  "Make sure the file is executable.")

;;;; Code Browsing
;; Commands
;; C-c f ffap
;; C-c i imenu
;; C-c v imenu-tree
;; cperl-tags-hier-init
;; M-. find-tag
;; M-* pop-tag-mark
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c v") 'imenu-tree)
(global-set-key (kbd "C-c j") 'ffap)
(setq tags-table-list '("./TAGS" "../TAGS" "../../TAGS"))
(autoload 'imenu-tree "imenu-tree" "Show imenu tree" t)
(setq imenu-tree-auto-update t)
(eval-after-load "imenu"
  '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))
(autoload 'tags-tree "tags-tree" "Show TAGS tree" t)
;; A wonderful minibuffer completion mode
(partial-completion-mode 1)
(setq PC-include-file-path
      (delete-dups (append PC-include-file-path pde-perl-inc)))

;; without this module ffap don't working for cperl (diabolo)
(require 'ffap-perl-module)

(setq ffap-url-regexp
      (concat
       "\\`\\("
       "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
       "\\|"
       "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
       "\\)[^:]" ; fix for perl module, require one more character that not ":"
       ))
(add-to-list 'ffap-alist  '(cperl-mode . pde-ffap-locate))


;;;; Finding Documents
;; Commands
;; C-c h help-dwim
;; pde-pod-to-manpage
;; perldoc-tree
(global-set-key (kbd "C-c h") 'help-dwim)
(setq cperl-lazy-help-time 2)
(defun pde-perl-mode-hook ()
  (cperl-lazy-install))

;;;; Interactive shell
;; Commands
;; run-perl
;; inf-perl-send
;; inf-perl-send-line
;; inf-perl-send-region
;; inf-perl-load-file
(autoload 'run-perl "inf-perl" "Start perl interactive shell" t)

;;;; Debugger
;; Commands
;; perldb-ui
;; perldb-many-windows
;; perldb-restore-windows
;;
;;
(autoload 'perldb-ui "perldb-ui" "perl debugger" t)

;; (eval-after-load 'cperl-mode
;; '(progn
;;
;;    ;;-- @LanX - not related to your question, but perhaps useful?
;;    ;;     (define-key cperl-mode-map (kbd "RET")   'reindent-then-newline-and-indent)
;;    ;;     (define-key cperl-mode-map (kbd "C-M-h") 'backward-kill-word)
;;    ;;     (define-key 'help-command "P" 'cperl-perldoc-at-point)
;;
;;
;;    ;; experiment: ;; (setq cperl-dark-background "gray14")
;;
;;    (set-face-attribute 'cperl-array-face nil
;;                        :foreground  "#ffff88"              ;; "yellow3"  "#ffff88"
;;                        :background  'unspecified
;;                        :weight      'normal
;;                        :slant       'normal
;;                        )
;;    (set-face-attribute 'cperl-hash-face nil
;;                        :foreground  "#e08020"              ;; "DarkOrange3"  "#e080202
;;                        :background  'unspecified
;;                        :weight      'normal
;;                        :slant       'normal
;;                        )
;;    ))

(require 'perltidy)
(autoload 'perltidy "perltidy-mode" nil t)
(autoload 'perltidy-mode "perltidy-mode" nil t)

;; Makes perltidy-mode automatic for cperl-mode
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook 'perltidy-mode))

(defmacro mark-active ()
  "Xemax/emacs compatibility macro"
  (if (boundp 'mark-active)
      'mark-active
    '(mark)))

(defun perltidy ()
  "Run perltidy on the current region or buffer."
  (interactive)
  ;; Inexplicably, save-excursion doesn'r work here.
  (let ((orig-point (point)))
    (unless (mark-active) (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)
    (goto-char orig-point)))

;; (global-set-key "\C-ct" 'perltidy)
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook (lambda () (interactive) (local-set-key "\C-ct" 'perltidy))))
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook (lambda () (interactive) (local-set-key [f12] 'perltidy))))

;; (defvar perltidy-mode nil
;;   "Automaticaly 'perltidy' when saving.")

;; (make-variable-buffer-local 'perltidy-mode)
;; (defun perltidy-write-hook ()
;;   "Perltidys a buffer during 'write-file-hooks' for 'perltidy-mode'"
;;   (if perltidy-mode
;;       (save-excursion
;; 	(widen)
;; 	(mark-whole-buffer)
;; 	(not (perltidy)))
;;     nil))

;; (defun perltidy-mode (&optional arg)
;;   "Perltidy minor mode."
;;   (interactive "P")
;;   (setq perltidy-mode
;; 	(if (null arg)
;; 	    (not perltidy-mode)
;; 	  (> (prefix-numeric-value arg) 0)))
;;   (make-local-hook 'write-file-hooks)
;;   (if perltidy-mode
;;       (add-hook 'write-file-hooks 'perltidy-write-hook)
;;     (remove-hook 'write-file-hooks 'perltidy-write-hook)))

;; (if (not (assq 'perltidy-mode minor-mode-alist))
;;     (setq minor-mode-alist
;; 	  (cons '(perltidy " Perltidy")
;; 		minor-mode-alist)))

;; (eval-after-load "cperl-mode"
;;   '(add-hook 'cperl-mode-hook 'perltidy-mode))
(provide 'setup-perl)
