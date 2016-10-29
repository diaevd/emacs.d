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

(provide 'setup-clang)
