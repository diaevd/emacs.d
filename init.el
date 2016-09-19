;; .git
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
(require 'bash-completion)
(bash-completion-setup)
(add-to-list 'auto-mode-alist '("[^_[:alnum:]|[:space:]]\\.[^.]\\w+\\'" . sh-mode))
(global-set-key [f6] 'shell)

;;------------------------------------------------------------------------
;;
;; Beautyfier
;;
;; http://melpa.org/#/clang-format
;;------------------------------------------------------------------------
(require 'clang-format)
;; (global-set-key [f12] 'clang-format-buffer)
;; (global-set-key [S-f12] 'clang-format-region)
(add-hook 'c-mode-hook (lambda () (interactive) (local-set-key [f12] 'clang-format-buffer)))
(add-hook 'c-mode-hook (lambda () (interactive) (local-set-key [S-f12] 'clang-format-region)))
(add-hook 'c++-mode-hook (lambda () (interactive) (local-set-key [f12] 'clang-format-buffer)))
(add-hook 'c++-mode-hook (lambda () (interactive) (local-set-key [S-f12] 'clang-format-region)))

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
;; (global-set-key (kbd "C-c q") 'hs-toggle-hiding)
;; (global-set-key (kbd "C-c c") 'hs-hide-block)
;; (global-set-key (kbd "C-c e") 'hs-show-block)

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
;; delete trailing white space
(add-hook 'c-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
(add-hook 'c++-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
(add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-x w") 'delete-trailing-whitespace)))
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'c-mode-hook
;; 	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(global-set-key (kbd "C-c C-w") 'delete-trailing-whitespace)

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
(add-hook 'mql-mode-hook 'helm-gtags-mode)
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
     (define-key helm-gtags-mode-map (kbd "C-c t") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "C-c f") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;;------------------------------------------------------------------------
;; Function args
(add-to-list 'load-path "~/.emacs.d/libs/function-args")
(require 'function-args)
(fa-config-default)

;; Semantic (CEDET)
(set-default 'semantic-case-fold t)
;; ключает глобальную поддержку Semanticdb
(global-semanticdb-minor-mode t)
;; включает режим автоматического запоминания информации о редактируемых тагах,
;; так что вы можете перейти к ним позднее с помощью команды semantic-mrub-switch-tags
(global-semantic-mru-bookmark-mode t)
;; активирует подстветку первой строки текущего тага
(global-semantic-highlight-func-mode t)
;; активирует показ названия текущего тага в верхней строке буфера
(global-semantic-stickyfunc-mode t)
;; активирует использование стилей при показе тагов разных типов.
;; Набор стилей определяется списком semantic-decoration-styles;
;; (global-semantic-decoration-mode t)
;; активирует автоматический анализ кода в буферах когда Emacs "свободен"
;; и ожидает ввода данных от пользователя (idle time)
(global-semantic-idle-scheduler-mode t)
;; включает подсветку вхождений локальных переменных
;; чье имя совпадает с именем текущего тага;
(global-semantic-idle-local-symbol-highlight-mode t)
;; активирует показ возможных дополнений имен во время ожидания ввода.
;; Требует чтобы был включен global-semantic-idle-scheduler-mode
(global-semantic-idle-completions-mode t)
;; активирует показ информации о текущем таге во время ожидания ввода
;; Требует чтобы был включен global-semantic-idle-scheduler-mode
(global-semantic-idle-summary-mode t)
;; ;; включает показ элементов, которые не обработались текущими правилами парсера
;; (global-semantic-show-unmatched-syntax-mode t)
;; ;; включает показ в строке статуса состояния парсера;
;; (global-semantic-show-parser-state-mode t)
;; ;; включает показ изменений сделанных в буфере, но которые еще не были
;; ;; обработаны инкрементальным парсером.
;; (global-semantic-highlight-edits-mode t)

;; ;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Activate semanticLibraries
(semantic-mode 1)

(require 'eassist)

;; возможности по дополнению имен и показу информации о функциях и классах
(require 'semantic/ia)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; customisation of modes
(defun alexott/cedet-hook ()
  ;; (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  ;; (local-set-key (kbd "M-RET") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "M-RET") 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  ;;
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  ;; Впихиваем таги в auto-complete
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic)
  )

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(semantic-add-system-include "/usr/include" 'c-mode)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)
(semantic-add-system-include "~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/MetaTrader\\ 4/MQL4/Include" 'c++-mode)
(semantic-add-system-include "~/.wine/drive_c/Program\\ Files\\ \\(x86\\)/MetaTrader\\ 4/MQL4/Libraries" 'c++-mode)
(semantic-add-system-include "~/.wine/drive_c/Program\\ Files/Alpari\\ Limited\\ MT5/MQL5/Include" 'c++-mode)
(semantic-add-system-include "~/.wine/drive_c/Program Files (x86)/MetaTrader 4/MQL4/Include" 'c++-mode)
(semantic-add-system-include "~/.wine/drive_c/Program Files (x86)/MetaTrader 4/MQL4/Libraries" 'c++-mode)
(semantic-add-system-include "~/.wine/drive_c/Program Files/Alpari Limited MT5/MQL5/Include" 'c++-mode)
(semantic-add-system-include "~/src/FX/mt4-Include" 'c++-mode)
(semantic-add-system-include "~/src/FX/mt4-Include" 'c++-mode)

;; если вы хотите включить поддержку gnu global
;; (when (cedet-gnu-global-version-check t)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))

;; включить поддержку ctags для основных языков:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; (when (cedet-ectag-version-check t)
;;   (semantic-load-enable-primary-exuberent-ctags-support))

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
;; Org-Mode
;;
;;------------------------------------------------------------------------
(custom-set-variables
 '(org-agenda-files (list "~/Documents/org/work.org"
			  "~/Documents/org/home.org"
			  "~/Documents/org/links.org"
			  "~/Documents/org/daily.org"
			  ))
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-directory "~/Documents/org")
 '(org-return-follows-link t)
 '(org-log-done t)
 ;; '(org-startup-indented t)
 '(org-indent-mode t)
 ;; '(org-indent-indentation-per-level t)
 ;; '(org-hide-leading-stars t)
 '(org-adapt-indentation nil)
 '(org-startup-folded nil)
)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; (setq org-startup-indented t)
;; (setq org-indent-mode t)
;; (setq org-hide-leading-stars t)

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
  (find-file "~/Documents/org")
  (other-window 1)
  (find-file "~/Documents/org")
  (other-window 1)
  (other-window 1)
  (dedicated-mode)
  (end-of-buffer)                       ; Go to the end of buffer
  (outline-previous-visible-heading 1)  ; Find the last heading
  (org-cycle)                           ; Make subtree visible
  )

(global-set-key (kbd "<f2> o") 'my-org)

;; (global-set-key (kbd "C-c m") 'org-table-copy-down)
;; (global-set-key (kbd "C-c RET") 'org-insert-heading-after-current)
;(global-set-key (kbd "C-c C-c RET") 'org-insert-heading)

(add-hook 'org-mode-hook (lambda () (interactive) (local-set-key (kbd "C-c m") 'org-table-copy-down)))
;; (add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-RET") 'org-insert-heading-after-current)))
;; (add-hook 'org-mode-hook (lambda () (interactive) (local-set-key (kbd "<C-return>") 'org-insert-heading)))
;; (add-hook 'org-mode-hook (lambda () (interactive) (local-set-key [(control return)] 'org-insert-heading-after-current)))
;; (add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-\j") 'org-insert-heading-after-current)))


;;------------------------------------------------------------------------
;;
;; Mail
;;
;;------------------------------------------------------------------------
;; Something About Ourselves
(setq user-mail-address "diaevd@gmail.com")
(setq user-full-name  "Evgeny Duzhakov")

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq elmo-imap4-use-modified-utf7 t) ;; Чтобы понимал русские названия
(setq mime-edit-split-message nil)
(setq wl-folder-check-async t)

;; (setq wl-from "Evgeny Duzhakov ")
;; (setq elmo-imap4-default-user "diaevd"
;;       elmo-imap4-default-server "imap.gmail.com"
;;       elmo-imap4-default-port 993
;;       elmo-imap4-default-authenticate-type 'clear
;;       elmo-imap4-default-stream-type 'ssl
;;       elmo-imap4-use-modified-utf7 t
;;       ;;
;;       wl-message-id-domain "diaevd@gmail.com"
;;       wl-from "Evgeny Duzhakov "
;;       wl-smtp-posting-server "smtp.gmail.com"
;;       wl-smtp-connection-type 'starttls
;;       wl-smtp-posting-port 587
;;       wl-smtp-authenticate-type "plain"
;;       wl-smtp-posting-user "diaevd"
;;       wl-local-domain "gmail.com"
;;       ;;
;;       elmo-pop3-debug t
;;       ssl-certificate-verification-policy 1
;;       wl-default-folder "%inbox"
;;       wl-default-spec "%"
;;       wl-folder-check-async t
;;       wl-thread-indent-level 4
;;       wl-thread-have-younger-brother-str "+"
;;       wl-thread-youngest-child-str       "+"
;;       wl-thread-vertical-str             "|"
;;       wl-thread-horizontal-str           "-"
;;       wl-thread-space-str                " "
;;       wl-summary-widthnil
;;       ;; l-summary-line-format "%n%T%P %W %D-%M-%Y %h:%m %t%[%c %f% %] %s"
;;       ;; wl-message-buffer-prefetch-folder-type-list nil
;;       ;; mime-transfer-level 8
;;       mime-edit-split-message nil
;;       ;; mime-edit-message-max-length 32768
;;       ;; mime-header-accept-quoted-encoded-words t
;;       ;; mime-browse-url-function 'browse-url-conkeror
;;       ;; pgg-passphrase-cache-expiry 300
;;       ;; pgg-decrypt-automatically t
;;       ;; wl-message-ignored-field-list '("^.*")
;;       ;; wl-message-visible-field-list '("^From:" "^To:" "^Cc:"
;;       ;; 				      "^Date:" "^Subject:" "^User-Agent:" "^X-Mailer:")
;;       ;; wl-message-sort-field-list    wl-message-visible-field-list
;;       ;; wl-message-window-size '(1 . 3)
;;       ;; wl-folder-window-width 40
;;       ;; wl-draft-preview-attributes-buffer-lines 7
;;       wl-draft-config-alist
;;       '(
;; 	;; ((string-match "avenger" wl-draft-parent-folder)
;; 	;;  (wl-message-id-domain . "avenger@yandex.ru")
;; 	;;  (wl-from . "rigidus ")
;; 	;;  ("From" . "avenger@yandex.ru")
;; 	;;  ;; ("Fcc" . "%Sent:avenger@yandex.ru:993")
;; 	;;  (wl-smtp-posting-server . "smtp.yandex.ru")
;; 	;;  ;; (wl-smtp-connection-type . nil)
;; 	;;  (wl-smtp-connection-type . 'starttls)
;; 	;;  ;; (wl-smtp-connection-type . 'ssl)
;; 	;;  ;; (wl-smtp-posting-port . 25)
;; 	;;  ;; (wl-smtp-posting-port . 465)
;; 	;;  (wl-smtp-posting-port . 587)
;; 	;;  (wl-smtp-authenticate-type . "plain")
;; 	;;  (wl-smtp-posting-user . "avenger")
;; 	;;  (wl-local-domain . "yandex.ru")
;; 	;;  )
;; 	((string-match "diaevd" wl-draft-parent-folder)
;; 	 (wl-message-id-domain . "diaevd@gmail.com")
;; 	 (wl-from . "Evgeny Duzhakov ")
;; 	 ("From" . "diaved@gmail.com")
;; 	 ;; ("Fcc" . "%Sent:rigidus@imap.gmail.com:993")
;; 	 (wl-smtp-posting-server . "smtp.gmail.com")
;; 	 (wl-smtp-connection-type . 'starttls)
;; 	 (wl-smtp-posting-port . 587)
;; 	 (wl-smtp-authenticate-type . "plain")
;; 	 (wl-smtp-posting-user . "diaevd")
;; 	 (wl-local-domain . "gmail.com")
;; 	 )
;; 	)
;;       )

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; эта часть настроек для доступа к Gmail по IMAP
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "diaevd@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t)

;; тут настройки отвечающие за SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "diaevd@gmail.com"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-local-domain "gmail.com"
      wl-message-id-domain "smtp.gmail.com")

(setq wl-from "Evgeny Duzhakov <diaevd@gmail.com>"
      ;; настройки папок IMAP
      ;; если у вас в настройках gmail стоит русский язык то копируйте все как есть
      ;; gmail создает имена папок в зависимости от локали
      ;;       wl-default-folder "%inbox"
      ;;       wl-draft-folder   "%[Gmail]/Черновики"
      ;;       wl-trash-folder   "%[Gmail]/Корзина"
      ;;       wl-fcc            "%[Gmail]/Отправленные"
      wl-fcc-force-as-read    t
      wl-default-spec "%")

;; список правил для удаления писем - тут вы указываете какую папку использовать для
;; удаления сообщений в каждом ящике. Правила срабатывают по регулярному выражению,
;; применяемому к имени папки, в которой вы сейчас находитесь. Например папка для входящих
;; сообщений %INBOX:some_test/clear@imap.gmail.com:993! подпадает под регулярное выражение
;; "^%.*some_test", и сообщения удаленные из нее будут перемещаться в папку
;; [Gmail]/Корзина:some_test/clear@imap.gmail.com:993!
;; (setq wl-dispose-folder-alist
;;       '(
;; 	("^%.*diaevd" . "%[Gmail]/Корзина:diaevd/clear@imap.gmail.com:993!")
;; 	;;("^%.*rambler_test" . "%Trash:rambler_test/clear@imap.rambler.ru:993!")
;; 	))

;; список почтовых адресов, с которых вы планируете отправлять письма
;;(setq wl-user-mail-address-list (quote ("some_test@gmail.com" "rambler_test@rambler.ru" "mail_ru_test@mail.ru")))
;; (setq wl-user-mail-address-list (quote ("diaevd@gmail.com")))
;; чтобы не выдавало ошибку при старте из-за того, что smtp сервер по умолчанию не определен сразу
;; (setq wl-insert-message-id nil)

;; чтобы при создании нового сообщения отправитель подставлялся в
;; зависимости от того в какой папке вы сейчас находитесь
;; (add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
;; (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

;; указываем что выбор SMTP сервера для отправки должен осуществляться в зависимости от правил
;; (setq wl-draft-config-matchone t)

;; и вот эти правила и настройки применяющиеся в случае срабатывания одного из них
;; (setq wl-draft-config-alist
;;       '(
;; 	;; some_test@gmail.com
;; 	(
;; 	 (string-match "diaevd" wl-draft-parent-folder)
;; 	 (wl-message-id-domain . "diaevd@gmail.com")
;; 	 (wl-from "Evgeny Duzhakov <diaevd@gmail.com>")
;; 	 (wl-smtp-posting-server . "smtp.gmail.com")
;; 	 (wl-smtp-connection-type . 'starttls)
;; 	 (wl-smtp-posting-port . 587)
;; 	 (wl-smtp-authenticate-type . "plain")
;; 	 (wl-smtp-posting-user . "diaevd")
;; 	 (wl-local-domain . "smtp.gmail.com")
;; 	 )
;; 	;; rambler_test@rambler.ru
;; 	;; (
;; 	;;  (string-match "rambler_test" wl-draft-parent-folder)
;; 	;;  (wl-message-id-domain . "rambler_test@rambler.ru")
;; 	;;  (wl-from . "rambler_test ")
;; 	;;  ("From" . "rambler_test@rambler.ru")
;; 	;;  (wl-smtp-posting-server . "smtp.rambler.ru")
;; 	;;  (wl-smtp-connection-type . 'starttls)
;; 	;;  (wl-smtp-posting-port . 587)
;; 	;;  (wl-smtp-authenticate-type . "plain")
;; 	;;  (wl-smtp-posting-user . "rambler_test")
;; 	;;  (wl-local-domain . "smtp.rambler.ru")
;; 	;;  )
;; 	;; mail_ru@mail.ru
;; 	;; (
;; 	;;  (string-match "mail_ru" wl-draft-parent-folder)
;; 	;;  (wl-message-id-domain . "mail_ru@mail.ru")
;; 	;;  (wl-from . "mail_ru ")
;; 	;;  ("From" . "mail_ru@mail.ru")
;; 	;;  (wl-smtp-posting-server . "smtp.mail.ru")
;; 	;;  (wl-smtp-connection-type . 'starttls)
;; 	;;  (wl-smtp-posting-port . 587)
;; 	;;  (wl-smtp-authenticate-type . "plain")
;; 	;;  (wl-smtp-posting-user . "mail_ru")
;; 	;;  (wl-local-domain . "smtp.mail.ru")
;; 	;; )
;; 	))


(setq
 ;; чтобы интерфейсы был как у Thunderbird - слева панель папок,
 ;; справа список писем и прсомотр текущего сообшения
 wl-stay-folder-window t
 wl-folder-window-width 40

 ;; чтобы при просмотре сообщения не видеть слишком много ненужных полей
 wl-message-ignored-field-list '("^.*:")
 wl-message-visible-field-list
 '("^\\(To\\|Cc\\):"
   "^Subject:"
   "^\\(From\\|Reply-To\\):"
   "^Organization:"
   "^Message-Id:"
   "^\\(Posted\\|Date\\):"
   )
 wl-message-sort-field-list
 '("^From"
   "^Organization:"
   "^X-Attribution:"
   "^Subject"
   "^Date"
   "^To"
   "^Cc"))

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
(global-set-key (kbd "M-g l") 'go-to-line-and-column)
(global-set-key (kbd "M-g M-l") 'go-to-line-and-column-cond)
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g M-g") 'go-to-line-and-column-cond)

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
(font-lock-add-keywords 'mql-mode
			'(("SEEK_SET" . 'font-lock-builtin-face)))
(font-lock-add-keywords 'mql-mode
			'(("SEEK_CUR" . 'font-lock-builtin-face)))
(font-lock-add-keywords 'mql-mode
			'(("SEEK_END" . 'font-lock-builtin-face)))
(font-lock-add-keywords 'mql-mode
			'(("FileIsExist" . 'font-lock-function-name-face)))
(font-lock-add-keywords 'mql-mode
			'(("FolderCreate" . 'font-lock-function-name-face)))
(font-lock-add-keywords 'mql-mode
			'(("ResetLastError" . 'font-lock-function-name-face)))

(add-hook 'mql-mode-hook (lambda () (interactive) (local-set-key (kbd "C-c C-b") 'mql-compile-dispatcher)))
;; (add-hook 'mql-mode-hook 'turn-on-orgtbl)

;;------------------------------------------------------------------------
;;
;; Yaml mode
;;
;;------------------------------------------------------------------------
;; If you wish to have Return key automatically indent cursor on new line, add the following to emacs config:
(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map (kbd "C-<ret>") 'newline-and-indent)))

;;------------------------------------------------------------------------
;;
;; GO mode
;;
;;------------------------------------------------------------------------
;; (require 'go-autocomplete)

;;------------------------------------------------------------------------
;;
;; GIT (magit)
;;
;;------------------------------------------------------------------------
(autoload 'magit-status "magit" nil t)
;; (global-set-key (kbd "M-g r m") 'magit-status)
(global-set-key (kbd "M-g m") 'magit-status)

;;------------------------------------------------------------------------
;;
;; popup-switcher
;;
;;------------------------------------------------------------------------
(require 'popup-switcher)

;; (setq psw-in-window-center t)

(global-set-key [f7] 'psw-switch-buffer)
;; (global-set-key [f8] 'psw-switch-projectile-files)

(eval-after-load "eassist"
   '(global-set-key [f8] 'psw-switch-function)
   )

;; (eval-after-load "eassist"
;;    '(global-set-key (kbd "M-o") 'psw-switch-h-cpp))

;; (eval-after-load "eassist"
;;   '(global-set-key (kbd "M-m") 'psw-list-methods))

;;      (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
;;      (define-key c-mode-base-map (kbd "M-m") 'eassist-list-method))

(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

;; (global-set-key [f8] 'psw-switch-function)

;; (defun psw-switch-h-cpp ()
;;    (interactive)
;;    (psw-switcher
;;     :items-list (eassist-switch-h-cpp)
;;     :item-name-getter 'car
;;     :switcher (psw-compose 'goto-char 'cdr)))

(defun psw-list-methods ()
   (interactive)
   (psw-switcher
    :items-list (eassist-mode)
    :item-name-getter 'car
    :switcher (psw-compose 'goto-char 'cdr)))

(global-set-key [f8] 'psw-switch-function)
;; Redefine switch file/buffers to
(global-set-key (kbd "C-x C-f") 'psw-navigate-files)
(global-set-key (kbd "C-x C-b") 'psw-switch-buffer)
(global-set-key (kbd "C-<f8>") 'psw-list-methods)

;;------------------------------------------------------------------------
;;
;; Registers to open files
;;
;;------------------------------------------------------------------------
;;;; (global-set-key (kbd "<f6>") (lambda() (interactive) (find-file "~/.emacs")))
(set-register ?e (cons 'file "~/.emacs.d/init.el")) ;; open it with  C-x r j e
(set-register ?l (cons 'file "~/Documents/org/links.org")) ;; open it with  C-x r j l
