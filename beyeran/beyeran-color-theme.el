
;;;; color-theme ;;;;
(defun beyeran-color-theme ()
  (interactive)
  (color-theme-install
   '(beyeran-color-theme
      ((background-color . "#000000")
      (background-mode . light)
      (border-color . "#030303")
      (cursor-color . "#4f4f4f")
      (foreground-color . "#a6a6a6")
      (mouse-color . "black"))
     (fringe ((t (:background "#030303"))))
     (mode-line ((t (:foreground "#ffffff" :background "#242424"))))
     (region ((t (:background "#121212"))))
     (font-lock-builtin-face ((t (:foreground "#878787"))))
     (font-lock-comment-face ((t (:foreground "#434242"))))
     (font-lock-function-name-face ((t (:foreground "#d12700"))))
     (font-lock-keyword-face ((t (:foreground "#f75c08"))))
     (font-lock-string-face ((t (:foreground "#ffffff"))))
     (font-lock-type-face ((t (:foreground"#fd8c35"))))
     (font-lock-variable-name-face ((t (:foreground "#fc2222"))))
     (minibuffer-prompt ((t (:foreground "#f53f00" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))

(provide 'beyeran-color-theme)
