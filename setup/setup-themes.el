;;------------------------------------------------------------------------
;;
;; Faces
;;
;;------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq my-color-themes (list
                       'zenburn-dark    ;; my fixed zenburn
                       'distinguished   ;; also almost my choice
                       'arjen           ;; almost my choice
                       'ld-dark         ;; very good for choice
                       'jbeans          ;; and this
                       'midnight        ;; good but strings is red
                       'waher           ;; keywords undescored
                       'subdued
                       'ujelly
                       'billw           ;; too many yellow but good
                       'cherry-blossom  ;; very goog but to brightness contrast
                       'dakrone         ;; good with fog
                       'cyberpunk
                       'badger          ;; good but with fog
                       'danneskjold
                       'aanila          ;; one of the best
                       'dark-laptop     ;; to many orange
                       'clarity         ;; like aanila
                       'manoj-dark
                       'bliss           ;; good but too many fog
                       'blackboard      ;; just a best for winmode
                       'yoshi           ;; nice choice for winmode
                       'dark-krystal    ;; good for winmode
                       'zenburn
                       'kooten
                       'abyss
                       'assemblage
                       ;; ---
                       'paganini
                       'hamburg
                       'dorsey
                       'hickey
                       'wheatgrass
                       'dark-emacs
                       'tango-dark
                       'tsdh-dark
                       'wombat
                       'brin
                       'fogus
                       'granger
                       'junio
                       'mccarthy
                       'odersky
                       'ritchie
                       'spolsky
                       'wilson
                       'graham
                       'adwaita
                       'deeper-blue
                       'dichromacy
                       'leuven
                       'light-blue
                       'misterioso
                       'tango
                       'tsdh-light
                       'whiteboard
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
;; (global-set-key [f4] 'my-theme-cycle)

;; '(default ((t (:family "Droid Sans Mono" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
;; '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
;; '(default ((t (:family "Monospace" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
;; '(default ((t (:family "Source Code Pro Regular" :foundry "outline" :slant normal :weight normal :height 110 :width normal))))
;; '(default ((t (:family "Inconsolata" :foundry "outline" :slant normal :weight bold :height 118 :width normal))))
;; '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 120 :width normal))))
;; '(default ((t (:family "Anonymous Pro" :foundry "outline" :slant normal :weight bold :height 120 :width normal))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:family "Inconsolata" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
 ;; '(default ((t (:family "Droid Sans Mono" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
 ;; '(default ((t (:family "Monospace" :foundry "outline" :slant normal :weight normal :height 108 :width normal))))
 ;; '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight bold :height 128 :width normal))))
 '(default ((t (:family "Anonymous Pro" :foundry "outline" :slant normal :weight bold :height 119 :width normal))))
 ;;'(default ((t (:family "Hack" :foundry "outline" :slant normal :weight bold :height 113 :width normal))))
 '(cperl-array-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint ((t (:inherit font-lock-variable-name-face :slant italic :weight bold))))
 '(fa-face-hint-bold ((t (:inherit font-lock-variable-name-face :slant italic :weight bold)))))

(provide 'setup-themes)
