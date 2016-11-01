;;------------------------------------------------------------------------
;;
;; BASH
;;
;;------------------------------------------------------------------------
(require 'bash-completion)
(bash-completion-setup)
(add-to-list 'auto-mode-alist '("[^_[:alnum:]|[:space:]]\\.[^.]\\w+\\'" . sh-mode))
(global-set-key [f6] 'shell)

(provide 'setup-bash)
