
;;
;; file: beyeran-color-theme-mac.el
;;

(defun beyeran-color-theme-mac ()
  (interactive)
  (color-theme-install
   '(beyeran-color-theme-mac
      ((background-color . "#1f1f1f")
      (background-mode . light)
      (border-color . "#2b2b2b")
      (cursor-color . "#333333")
      (foreground-color . "#b5b5b5")
      (mouse-color . "black"))
     (fringe ((t (:background "#2b2b2b"))))
     (mode-line ((t (:foreground "#000000" :background "#666666"))))
     (region ((t (:background "#999999"))))
     (font-lock-builtin-face ((t (:foreground "#aa37e6"))))
     (font-lock-comment-face ((t (:foreground "#3b3b3b"))))
     (font-lock-function-name-face ((t (:foreground "#6b6b6b"))))
     (font-lock-keyword-face ((t (:foreground "#ff362e"))))
     (font-lock-string-face ((t (:foreground "#1b79fe"))))
     (font-lock-type-face ((t (:foreground"#3fb819"))))
     (font-lock-variable-name-face ((t (:foreground "#999999"))))
     (minibuffer-prompt ((t (:foreground "#7299ff" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))

(provide 'beyeran-color-theme-mac)
