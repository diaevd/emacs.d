(require 'cc-mode)
(require 'semantic)

(require 'swiper-helm)
(require 'function-args)
(fa-config-default)
(eval-after-load 'function-args
   '(progn
      (define-key function-args-mode-map (kbd "C-M-l") 'moo-jump-local)
      (define-key function-args-mode-map (kbd "C-M-j") 'moo-jump-directory) ;;))
      (define-key moo-jump-keymap (kbd "C-M-l") 'moo-jump-local)
      (setq moo-select-method 'helm-fuzzy)))

(set-default 'semantic-case-fold t)
;; ключает глобальную поддержку Semanticdb
(global-semanticdb-minor-mode t)
;; включает режим автоматического запоминания информации о редактируемых тагах,
;; так что вы можете перейти к ним позднее с помощью команды semantic-mrub-switch-tags
;; (global-semantic-mru-bookmark-mode t)
;; ;; активирует подстветку первой строки текущего тага
(global-semantic-highlight-func-mode t)
;; активирует показ названия текущего тага в верхней строке буфера
(global-semantic-stickyfunc-mode t)
; активирует использование стилей при показе тагов разных типов.
;; Набор стилей определяется списком semantic-decoration-styles;
;; (global-semantic-decoration-mode t)	;
;; активирует автоматический анализ кода в буферах когда Emacs "свободен"
;; и ожидает ввода данных от пользователя (idle time)
(global-semantic-idle-scheduler-mode t)
;; включает подсветку вхождений локальных переменных
;; чье имя совпадает с именем текущего тага;
(global-semantic-idle-local-symbol-highlight-mode t)
;; ;; активирует показ возможных дополнений имен во время ожидания ввода.
;; ;; Требует чтобы был включен global-semantic-idle-scheduler-mode
(global-semantic-idle-completions-mode t)
;; активирует показ информации о текущем таге во время ожидания ввода
;; Требует чтобы был включен global-semantic-idle-scheduler-mode
;; (global-semantic-idle-summary-mode t)
;; ;; включает показ элементов, которые не обработались текущими правилами парсера
;; (global-semantic-show-unmatched-syntax-mode t)
;; ;; включает показ в строке статуса состояния парсера;
;; (global-semantic-show-parser-state-mode t)
;; ;; включает показ изменений сделанных в буфере, но которые еще не были
;; ;; обработаны инкрементальным парсером.
;; (global-semantic-highlight-edits-mode t)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)
(add-hook 'mql-mode-hook 'alexott/cedet-hook)

(semantic-mode 1)

(require 'stickyfunc-enhance)
;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

(provide 'setup-cedet)
