;;------------------------------------------------------------------------
;;
;; Beautyfier
;;
;; http://melpa.org/#/clang-format
;;------------------------------------------------------------------------
(use-package 'clang-format
  :config
;; (global-set-key [f12] 'clang-format-buffer)
;; (global-set-key [S-f12] 'clang-format-region)
  (add-hook 'c-mode-hook (lambda () (interactive) (local-set-key [f12] 'dia/reformat-buffer)))
  (add-hook 'c-mode-hook (lambda () (interactive) (local-set-key [S-f12] 'dia/reformat-region)))
  (add-hook 'c++-mode-hook (lambda () (interactive) (local-set-key [f12] 'dia/reformat-buffer)))
  (add-hook 'c++-mode-hook (lambda () (interactive) (local-set-key [S-f12] 'dia/reformat-region))))

(provide 'setup-clang)
