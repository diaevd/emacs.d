;;(setq user-mail-address "diaevd@gmail.com")
(setq-default major-mode 'text-mode)
(setq column-number-mode t)
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
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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

;; (add-to-list 'load-path "~/.emacs.d/elpa/smartparens")
;; (require 'smartparens-config)
;; (show-smartparens-global-mode +1)
;; (smartparens-global-mode -1)

;; (use-package smartparens-config
;;    :ensure smartparens
;;    :config
;;    (progn
;;      (show-smartparens-global-mode t)))

;; (sp-pair "'" nil :actions :rem)
;; (sp-pair "\"" nil :actions :rem)
;; (sp-pair "(" nil :actions :rem)
;; (sp-pair "{" nil :actions :rem)

;; (add-hook 'c-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; when you press RET, the curly braces automatically
;; add another newline
;;(sp-with-modes '(c-mode c++-mode)
;;  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;;  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
;;                                            ("* ||\n[i]" "RET"))))

;;------------------------------------------------------------------------
;;
;; Fuck off the smartparens we can use beter show-paren-mode or highlight-parentheses
;;
;;------------------------------------------------------------------------
(require 'highlight-parentheses)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode t)

;;(show-paren-mode 1)
;; (require 'paren)
;; (set-face-background 'show-paren-match (face-background 'default))
;; (set-face-foreground 'show-paren-match "#def")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;;------------------------------------------------------------------------

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
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ;; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ;; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ;; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ;; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
;;------------------------------------------------------------------------
;;
;; Visual parts
;;
;;------------------------------------------------------------------------
(global-set-key (kbd "C-c C-v") 'imenu-tree)
(global-set-key (kbd "C-c C-f") 'helm-find-files)
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

;;------------------------------------------------------------------------
;;
;; Mail
;;
;;------------------------------------------------------------------------
(require 'mu4e)

;; default
;; (setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
	 ("/[Gmail].Sent Mail"   . ?s)
	 ("/[Gmail].Trash"       . ?t)
	 ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 900)             ;; update every 15 minutes.


;; mu4e-action-view-in-browser is built into mu4e
;; by adding it to these lists of custom actions
;; it can be invoked by first pressing a, then selecting
;;(add-to-list ‘mu4e-headers-actions ‘("in browser" . mu4e-action-view-in-browser) t)
;;(add-to-list ‘mu4e-view-actions ‘("in browser" . mu4e-action-view-in-browser) t)

;; don’t keep message buffers around
(setq message-kill-buffer-on-exit t)
;; attachments go here
(setq mu4e-attachment-dir "~/Downloads")


;; something about ourselves
(setq
 user-mail-address "diaevd@gmail.com"
 user-full-name  "Evgeny Duzhakov"
 mu4e-compose-signature
 (concat
  "WBR,\n"
  "Evgeny Duzhakov aka diabolo"
  "Skype: diaevd\n"
  "ICQ: 5176006\n"))

(setq message-kill-buffer-on-exit t)
;; Use fancy chars
(setq mu4e-use-fancy-chars t)

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

;; when you want to use some external command for html->text
;; conversion, e.g. the ‘html2text’ program
;; (cpbotha: html2text sees to work better than the built-in one)
(setq mu4e-html2text-command "html2text -utf8")

;; Silly mu4e only shows names in From: by default. Of course we also
;; want the addresses.
(setq mu4e-view-show-addresses t)

;; Re-index every 15 minutes.
(setq mu4e-update-interval (* 15 60))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "diaevd@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;;------------------------------------------------------------------------
;;
;; Goto functions
;;
;;------------------------------------------------------------------------
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

;;;;;;;; (string-match "\\`\\([0-9]*\\):\\([0-9]*\\)\\'\\)\\)'" line))
(defun go-to-line-and-column (line column)
  (interactive "nLine: \nnColumn: ")
  (goto-line line)
  (move-to-column column)
  )

(defun go-to-line-and-column-cond (lc-cond)
  "Allow a specification of LINE:COLUMN or LINE,COLUMN instead of just COLUMN.
Just :COLUMN or ,COLUMN moves to the specified column on the current line.
LINE alone still moves to the beginning of the specified line (like LINE:0 or LINE,0).
By Default I'm bind it to M-g M-l.
The default value of the COLUMN is decrement by -1 
because all compilers consider the number of COLUMN from 1 (just for copy-past)"
  (interactive "sLine,Column: ")
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
(global-set-key (kbd "M-g l") 'go-to-line-and-column)
(global-set-key (kbd "M-g M-l") 'go-to-line-and-column-cond)
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g M-g") 'go-to-line-and-column-cond)

;;------------------------------------------------------------------------
;;
;; Org-Mode
;;
;;------------------------------------------------------------------------
(custom-set-variables
 '(org-agenda-files (list "~/Documents/org/work.org"
			  "~/Documents/org/home.org"
			  "~/Documents/org/links.org"
			  ))
 '(org-default-notes-file "~/Documents/org/notes")
 '(org-directory "~/Documents/org/org")
 '(org-return-follows-link t)
)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(defun my-org ()
  "Create org-mode windows open files"
  (interactive)
  (setq default-frame-alist
	`((width . ,140)
	  (height . ,45)
	  (top . ,5)
	  (left . ,5)
	  (user-position . t)
	  ))
  (delete-other-windows)
  (split-window-horizontally)
  (setq eik-links-win-w 60)
  (shrink-window-horizontally 32)
  (find-file "~/Documents/org/links.org")
  (dedicated-mode)
  (other-window 1)
  (find-file "~/Documents/org/daily.org")
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 1)
  (find-file "~/")
  (other-window 1)
  (find-file "~/")
  (other-window 1)
  (other-window 1)
  (dedicated-mode)
  (end-of-buffer)                       ; Go to the end of buffer
  (outline-previous-visible-heading 1)  ; Find the last heading
  (org-cycle)                           ; Make subtree visible								  )
  )

(global-set-key (kbd "<f2> o") 'my-org)
(global-set-key (kbd "C-c m") 'org-table-copy-down)

;;------------------------------------------------------------------------
;;
;; Faces
;;
;;------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (load-theme dark-emacs t t)
;; (load-theme cyberpunk t t)
;; (load-theme adwaita t t)
;; (load-theme deeper-blue t t)
;; (load-theme dichromacy t t)
;; (load-theme leuven t t)
;; (load-theme light-blue t t)
;; (load-theme manoj-dark t t)
;; (load-theme misterioso t t)
;; (load-theme tango-dark t t)
;; (load-theme tango t t)
;; (load-theme tsdh-dark t t)
;; (load-theme tsdh-light t t)
;; (load-theme wheatgrass t t)
;; (load-theme whiteboard t t)
;; (load-theme wombat t t)

(setq my-color-themes (list
		       'dark-emacs
		       'Adwaita
		       'Deeper-Blue
		       'Dichromacy
		       'Leuven
		       'Light-Blue
		       'Manoj-Dark
		       'misterioso
		       'tango-dark
		       'tango
		       'tsdh-dark
		       'tsdh-light
		       'wheatgrass
		       'whiteboard
		       'wombat
		       ))

(defun my-theme-set-default () ;; Set the first row
  (interactive)
  (setq theme-current my-color-themes)
  (load-theme (car theme-current) t nil))

(defun my-describe-theme () ;; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))

(defun my-theme-cycle () ;; Set the next theme (fixed by Chris Webber - tanks)
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (load-theme (car theme-current) t nil)
  (message "%S" (car theme-current)))

(setq theme-current my-color-themes)
(setq color-theme-is-global nil) ; Initialization
(my-theme-set-default)
(global-set-key [f4] 'my-theme-cycle)

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

(add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-c C-b") 'mql-compile-dispatcher)))

;;------------------------------------------------------------------------
;;
;; Registers to open files
;;
;;------------------------------------------------------------------------
;;;; (global-set-key (kbd "<f6>") (lambda() (interactive) (find-file "~/.emacs")))
(set-register ?e (cons 'file "~/.emacs.d/init.el")) ;; open it with  C-x r j e
(set-register ?l (cons 'file "~/Documents/org/links.org")) ;; open it with  C-x r j l
