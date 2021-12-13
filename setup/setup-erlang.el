;;
;;
;;

(use-package 'erlang-start
  :config
  (add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
  (defun my-erlang-mode-hook ()
    ;; when starting an Erlang shell in Emacs, default in the node name
    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    ;; add Erlang functions to an imenu menu
    ;;(imenu-add-to-menubar "imenu")
    ;; customize keys
    (local-set-key [return] 'newline-and-indent))
  ;; Some Erlang customizations
  (add-hook 'erlang-mode-hook 'my-erlang-mode-hook))

(provide 'setup-erlang)
