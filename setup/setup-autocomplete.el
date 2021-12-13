;;------------------------------------------------------------------------
;;
;; auto complete mode
;;
;;------------------------------------------------------------------------
(use-package 'auto-complete-config
  :config
  (add-to-list 'ac-dictionary-directories (expand-file-name "ac-dict" user-emacs-directory))
  (ac-config-default)
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB")
  (setq ac-auto-start nil)
  (defun iqbal-setup-cedet-auto-completion ()
    (add-to-list 'ac-sources 'ac-source-semantic))
  (add-hook 'c-mode-common-hook 'iqbal-setup-cedet-auto-completion)
;; Make sure "Anything" is available
;;(require 'anything)
;;(require 'anything-match-plugin)
  )


(provide 'setup-autocomplete)
