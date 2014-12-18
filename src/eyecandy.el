
;;
;; color theme
;;

;; (add-theme "sunburst")
;; (load-theme 'sunburst t)
;; (add-theme "monokai")
;; (load-theme 'monokai t)
;; (add-to-list 'load-path  "~/.emacs.d/color-theme/tomorrow")
;; (require 'color-theme-sanityinc-tomorrow)
;; (color-theme-sanityinc-tomorrow-bright)
(add-to-list 'custom-theme-load-path "~/.emacs.d/modules/color-themes/themes")
(load-theme 'graham t)

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
(set-face-attribute 'default nil :font "Source Code Pro-8")
;; (set-default-font "Droid Sans Mono-9")
