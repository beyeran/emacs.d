
;;
;; file: beyeran-green-hell-mac.el
;;

(defun green-hell-mac ()
  (interactive)
  (color-theme-install
   '(green-hell-mac
      ((background-color . "#000000")
      (background-mode . light)
      (border-color . "#000000")
      (cursor-color . "#063d00")
      (foreground-color . "#a8a8a8")
      (mouse-color . "black"))
     (fringe ((t (:background "#000000"))))
     (mode-line ((t (:foreground "#525252" :background "#000000"))))
     (region ((t (:background "#212121"))))
     (font-lock-builtin-face ((t (:foreground "#62f000"))))
     (font-lock-comment-face ((t (:foreground "#333333"))))
     (font-lock-function-name-face ((t (:foreground "#00ed14"))))
     (font-lock-keyword-face ((t (:foreground "#279900"))))
     (font-lock-string-face ((t (:foreground "#729fcf"))))
     (font-lock-type-face ((t (:foreground"#8ae234"))))
     (font-lock-constant-face ((t (:foreground "#757b23"))))
     (font-lock-variable-name-face ((t (:foreground "#6e6e6e"))))
     (minibuffer-prompt ((t (:foreground "#22f500" :bold t))))
     (font-lock-warning-face ((t (:foreground "red" :bold t))))
     )))
(provide 'green-hell-mac)
