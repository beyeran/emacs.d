
;;;; color-theme ;;;;
(defun beyeran-color-theme ()
  (interactive)
  (color-theme-install
   '(beyeran-color-theme
      ((background-color . "#101010")
      (background-mode . dark)
      (border-color . "#121212")
      (cursor-color . "#b0b0b0")
      (foreground-color . "#dedede")
      (mouse-color . "black"))
     (fringe ((t (:background "#121212"))))
     (mode-line ((t (:foreground "#393939" :background "#121212"))))
     (mode-line-inactive ((t (:foreground "#393939" :background "#101010"))))
     (region ((t (:background "#020202"))))
     (font-lock-builtin-face ((t (:foreground "#da4939"))))
     (font-lock-comment-face ((t (:foreground "#202020"))))
     (font-lock-function-name-face ((t (:foreground "#ff6c29a"))))
     (font-lock-keyword-face ((t (:foreground "#da4939"))))
     (font-lock-string-face ((t (:foreground "#509f7e"))))
     (font-lock-type-face ((t (:foreground "#6d9cbe"))))
     (font-lock-variable-name-face ((t (:foreground "#5e468c"))))
     (minibuffer-prompt ((t (:foreground "#435d75" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))

(beyeran-color-theme)

(provide 'beyeran-color-theme)
