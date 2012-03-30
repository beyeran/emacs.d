
(require 'scss-mode)

(sys-diversification
 (setq scss-sass-command "~/.rvm/gems/ruby-1.9.3-p0/bin/sass")
 ())

(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))

(provide 'beyeran-scss-mode)
