
(require 'quack)
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))

(setq scheme-program-name "guile")

(add-to-list 'Info-default-directory-list (concat extensions-dir "scheme/info/"))

(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map [f1]
              '(lambda ()
                 (interactive)
                 (ignore-errors
                   (let ((symbol (thing-at-point 'symbol)))
                        (info "(r5rs)")
                        (Info-index symbol)))))))

(provide 'beyeran-scheme-mode)
