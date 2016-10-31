(when window-system (add-to-list 'default-frame-alist '(width . 208))
      (add-to-list 'default-frame-alist '(height . 56))
      (setq inhibit-splash-screen t)	;; disable splash screan
      (setq inhibit-startup-message t)	;; ...
      ;; (setq split-height-threshold nil) ;; in window mode split horizontaly
      ;; (setq split-width-threshold 0)	;; ...
      (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)
      (tool-bar-mode -1)		;; disable tool bar
      )

(defun 2-windows-vertical-to-horizontal ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))
;; (add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

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

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; MELPA use-package
(require 'use-package)
(setq use-package-always-ensure t)

<<<<<<< HEAD
;; place for my libs
(add-to-list 'load-path "~/.emacs.d/libs")
(add-to-list 'load-path "~/.emacs.d/custom")
=======
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
>>>>>>> 2028ede... * .gitignore: Add #*# files

(byte-recompile-directory (expand-file-name "~/.emacs.d/custom") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/libs") 0)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/themes") 0)

(load "profile-dotemacs")
(defvar profile-dotemacs-file "~/.emacs.d/init.el" "File to be profiled.")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-themes)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-bash)
(require 'setup-clang)
(require 'setup-perl)
(require 'setup-hideshow)
(require 'setup-mql)
(require 'setup-erlang)
(require 'setup-autocomplete)
(require 'setup-popup-switcher)
(require 'setup-git)
(require 'setup-functions)

;;------------------------------------------------------------------------
;;
;; All global keys place here because maybe currupted by prev setups
;;
;;------------------------------------------------------------------------
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r") 'helm-recentf)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-h o") 'helm-occur)
(global-set-key (kbd "C-h w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-h g") 'helm-google-suggest)
(global-set-key (kbd "C-h x") 'helm-register)
;;
(global-set-key (kbd "C-M-k") 'kill-sexp)
;;
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c v") 'imenu-tree)
(global-set-key (kbd "C-c j") 'ffap)
(setq imenu-tree-auto-update 't)
(setq imenu-auto-rescan 't)
(eval-after-load "imenu"
  '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))
<<<<<<< HEAD

;;------------------------------------------------------------------------
;;
;; Registers to open files
=======
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
>>>>>>> 2028ede... * .gitignore: Add #*# files
;;
;;------------------------------------------------------------------------
;;;; (global-set-key (kbd "<f6>") (lambda() (interactive) (find-file "~/.emacs")))
(set-register ?e (cons 'file "~/.emacs.d/init.el")) ;; open it with  C-x r j e
(set-register ?l (cons 'file "~/Documents/org/links.org")) ;; open it with  C-x r j l
(set-register ?o (cons 'file "~/Documents/org/notes.org")) ;; open it with  C-x o j 0

;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/usr/src/zen/zen-kernel")))
 '(org-agenda-files
   (quote
    ("~/Documents/org/notes.org" "~/Documents/org/work.org" "~/Documents/org/home.org" "~/Documents/org/links.org" "~/Documents/org/daily.org")))
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-directory "~/Documents/org")
 '(org-indent-mode t)
 '(org-log-done t)
 '(org-return-follows-link t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (git-blamed helm-projectile erlang clang-format bug-hunter magit sr-speedbar swiper-helm sublime-themes ggtags function-args zygospore helm-gtags helm yasnippet ws-butler volatile-highlights use-package undo-tree iedit dtrt-indent counsel-projectile company clean-aindent-mode anzu)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Anonymous Pro" :foundry "outline" :slant normal :weight bold :height 120 :width normal))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
<<<<<<< HEAD
 '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold)))))
=======
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
>>>>>>> 2028ede... * .gitignore: Add #*# files
