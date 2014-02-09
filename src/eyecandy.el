
;;
;; color theme
;;

;; (add-theme "sunburst")
;; (load-theme 'sunburst t)
(add-theme "monokai")
(load-theme 'monokai t)

;;
;; hud
;;
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-visual-line-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(setq inhibit-splash-screen t)
(setq visible-bell t)

;;
;; font
;;
(set-face-attribute 'default nil :font "Droid Sans Mono-10")
;;(set-default-font "Droid Sans Mono-9")
