;;------------------------------------------------------------------------
;;
;; auto complete mode
;;
;;------------------------------------------------------------------------
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
;; Make sure "Anything" is available
(require 'anything)
(require 'anything-match-plugin)

(defun iqbal-setup-cedet-auto-completion ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'iqbal-setup-cedet-auto-completion)

(provide 'setup-autocomplete)
