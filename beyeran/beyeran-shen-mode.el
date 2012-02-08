
(require 'shen-mode)
(require 'inf-shen)

(add-to-list 'auto-mode-alist '("\\.shen$" . shen-mode))

(setq inferior-shen-program "/usr/bin/shen")

(provide 'beyeran-shen-mode)
