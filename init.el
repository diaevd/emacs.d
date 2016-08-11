(setq user-mail-address "diaevd@gmail.com")
(setq-default major-mode 'text-mode)
;; place for my libs
(add-to-list 'load-path "~/.emacs.d/libs")

(global-set-key [f9] 'menu-bar-open) ;; Sometimes need

(setq-default indent-tabs-mode 't)
(setq indent-tabs-mode t)
(setq-default tab-width 8)
(setq indent-line-function 'insert-tab)
(setq sh-basic-offset 8)
(setq sh-indentation 8)

(setq-default c-basic-offset 8
              c-indentation 8
              indent-tabs-mode 't
              c-default-style "linux")

;;------------------------------------------------------------------------
;;
;; MELPA
;;
;;------------------------------------------------------------------------
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
            '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; MELPA use-package
(require 'use-package)

;;------------------------------------------------------------------------
;;
;; auto complete mode
;;
;;------------------------------------------------------------------------
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

;;------------------------------------------------------------------------
;;
;; BASH
;;
;;------------------------------------------------------------------------
;; (require 'bash-completion)
;; (bash-completion-setup)

;;------------------------------------------------------------------------
;;
;; Beautyfier
;;
;; http://melpa.org/#/clang-format
;;------------------------------------------------------------------------
(require 'clang-format)
(global-set-key [f12] 'clang-format-buffer)
(global-set-key [S-f12] 'clang-format-region)


;;------------------------------------------------------------------------
;;
;; Goto functions
;;
;;------------------------------------------------------------------------
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(defadvice goto-line (around goto-column activate)
  "Allow a specification of LINE:COLUMN instead of just COLUMN.
Just :COLUMN moves to the specified column on the current line.
Just LINE: moves to the current column on the specified line.
LINE alone still moves to the beginning of the specified line (like LINE:0)."
  (if (symbolp line) (setq line (symbol-name line)))
  (let ((column (save-match-data
		  (if (and (stringp line)
			   (string-match "\\`\\([0-9]*\\):\\([0-9]*\\)\\'" line))
		      (prog1
			  (match-string 2 line)
			(setq line (match-string 1 line)))
		    nil))))
    (if (stringp column)
	(setq column (if (= (length column) 0)
			 (current-column)
		       (string-to-int column))))
    (if (stringp line)
	(setq line (if (= (length line) 0)
		       (if buffer
			   (save-excursion
			     (set-buffer buffer)
			     (line-number-at-pos))
			 nil)
		     (string-to-int line))))
    (if line
	ad-do-it)
    (if column
	(let ((limit (- (save-excursion (forward-line 1) (point))
			(point))))
	  (when (< column limit)
	    (beginning-of-line)
	    (forward-char column))))))

(global-set-key (kbd "M-g M-c") 'go-to-column)

;;(global-set-key (kbd "M-g l") 'goto-line)
;;------------------------------------------------------------------------
;; Make sure "Anything" is available
(require 'anything)
(require 'anything-match-plugin)

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
(ido-mode 1)
(require 'template-simple)

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
(global-set-key (kbd "M-;") 'hippie-expand)
(global-set-key (kbd "C-;") 'comment-dwim)
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

(defun pde-perl-mode-hook ()
  ;; chmod when saving
  (when (and buffer-file-name
	     (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
    (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil))

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

;; Rebinding keys for hideshow
(require 'hideshow)
(define-key hs-minor-mode-map "\C-c\C-o"
  (let ((map (lookup-key hs-minor-mode-map "\C-c@")))
    ;; C-h is help to remind me key binding
    (define-key map "\C-h" 'describe-prefix-bindings)
    (define-key map "\C-q" 'hs-toggle-hiding)
    ;; compatible with outline
    (define-key map "\C-c" 'hs-hide-block)
    (define-key map "\C-e" 'hs-show-block)
    map))

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

;;------------------------------------------------------------------------
;;
;; don't know who is, but somebody from pde set indent-tabs-mode to nil
;;
;;------------------------------------------------------------------------
(setq-default indent-tabs-mode 't)

;;------------------------------------------------------------------------
;;
;; C/C++
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Package: clean-aindent-mode
;;(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
(defun my-pkg-init()
  (electric-indent-mode -1)  ; no electric indent, auto-indent is sufficient
  (clean-aindent-mode t)
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent))
(add-hook 'after-init-hook 'my-pkg-init)

;; Package: dtrt-indent
(add-to-list 'load-path "~/.emacs.d/elpa/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(add-to-list 'load-path "~/.emacs.d/elpa/ws-butler")
(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)

;; useful snippets
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; http://melpa.org/#/yasnippet
;; for yasnippets
(add-to-list 'load-path "~/.emacs.d/elpa/dropdown-list")
(require 'dropdown-list)
(setq yas-prompt-functions
      '(yas-dropdown-prompt
        yas-ido-prompt
        yas-x-prompt
        yas-completing-prompt
        yas-no-prompt))

;; Package: smartparens

(add-to-list 'load-path "~/.emacs.d/elpa/smartparens")
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; Compilation Support
;; C-o Display matched location, but do not switch point to matched buffer
;; M-n Move to next error message, but do not visit error location
;; M-p Move to next previous message, but do not visit error location
;; M-g n Move to next error message, visit error location
;; M-g p Move to previous error message, visit error location
;; RET Visit location of error at poiint
;; M-{ Move point to the next error message or match occurring in a different file
;; M-} Move point to the previous error message or match occurring in a different file
;; q Quit *compilation* buffer
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; Debugging
;; 1. GUD interaction buffer
;; 2. Locals/Registers buffer
;; 3. Primary Source buffer
;; 4. I/O buffer for debugging program
;; 5. Stack buffer6. Breakpoints/Threads buffer
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq auto-window-vscroll nil) ;; disable auto scrolling
(setq-default scroll-margin 1)
(setq-default scroll-step 1) ;; keyboard scroll one line at a time
(setq-default scroll-conservatively 0) ;; lags fix (like ssh sessions)
(setq-default scroll-up-aggressively 0.01)
(setq-default scroll-down-aggressively 0.01)

;; shift+arrows_keys is for select by default, however change it so
;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)
;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )


;;------------------------------------------------------------------------
;;
;; TAGS
;;
;;------------------------------------------------------------------------
(require 'helm)

(custom-set-variables
 '(helm-gtags-prefix-key "C-t")
 '(helm-gtags-suggested-key-mapping t))

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-display-style t)
 )

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/libs/function-args")
(require 'function-args)
(fa-config-default)
(set-default 'semantic-case-fold t)
(semantic-add-system-include "/usr/include" 'c-mode)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)

;;------------------------------------------------------------------------
;; (use-package helm-mode
;;   :ensure helm
;;   :config (helm-mode 1)
;;   :bind (("M-x"       . undefined)
;; 	 ("M-x"       . helm-M-x)
;; 	 ("M-y"       . helm-show-kill-ring)
;; 	 ("C-x C-b"   . helm-buffers-list)
;; 	 ("C-x C-f"   . helm-find-files)
;; 	 ("C-c <SPC>" . helm-all-mark-rings)
;; 	 ("C-x r b"   . helm-filtered-bookmarks)
;; 	 ("C-h r"     . helm-info-emacs)
;; 	 ("C-:"       . helm-eval-expression-with-eldoc)
;; 	 ("C-,"       . helm-calcul-expression)
;; 	 ("C-h i"     . helm-info-at-point)
;; 	 ("C-x C-d"   . helm-browse-project)
;; 	 ("<f1>"      . helm-resume)
;; 	 ("C-h C-f"   . helm-apropos)
;; 	 ("C-h a"     . helm-apropos)
;; ;;	 ("<f5> s"    . helm-find)
;; 	 ("<f2>"      . helm-execute-kmacro)
;; 	 ("C-c i"     . helm-imenu-in-all-buffers)
;; 	 ("C-s"       . helm-occur)
;; 	 ))

;; (use-package helm-adaptive
;;   :config (helm-adaptive-mode 1))

;; (use-package helm-ring
;;   :config (helm-push-mark-mode 1))

;; (use-package helm-utils
;;   ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
;;   :config (helm-popup-tip-mode 1))

;; (use-package helm-sys
;;   :config (helm-top-poll-mode 1))

;; (use-package projectile
;;   :ensure projectile
;;   :bind (("C-h f" . helm-projectile))
;;   )

;; (use-package helm-projectile
;;   :ensure helm-projectile
;;   :bind (("C-h s" . helm-do-ag))
;;   )

;; (use-package helm-ag
;;   :ensure helm-ag)

;;------------------------------------------------------------------------
;;
;; MQL
;;
;;;; -- https://github.com/kostafey/kostafeys-emacs-confik/blob/master/artifacts/mql-mode.el
;; https://github.com/jianingy/emacs-ysl/blob/master/site-lisp/extra/mql-mode.el
;;------------------------------------------------------------------------
;;(load "~/.emacs.d/mql-mode.el")
(load "mql-mode")
(modify-coding-system-alist 'file "\\.mql\\'" 'windows-1251)
(modify-coding-system-alist 'file "\\.mq4\\'" 'windows-1251)
(modify-coding-system-alist 'file "\\.mq5\\'" 'windows-1251)
(modify-coding-system-alist 'file "\\.mqh\\'" 'windows-1251)

(font-lock-add-keywords 'mql-mode
			'(("input" . 'font-lock-keyword-face)))
(font-lock-add-keywords 'mql-mode
			'(("sinput" . 'font-lock-keyword-face)))

;;------------------------------------------------------------------------
;;
;; Customize comments
;;
;;------------------------------------------------------------------------
;;(add-to-list 'auto-mode-alist '("\\.[ch]\\'" . c++-mode))
(add-to-list 'load-path "~/.emacs.d/elpa/comment-dwim-2")
(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
					comment-end   "")))

;; Don't ask me additional about unsaved buffers
(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;;;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;; '(fa-face-semi ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;; '(fa-face-type-compound ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;; '(fa-face-type-definition ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
;; '(fa-face-type ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 )


