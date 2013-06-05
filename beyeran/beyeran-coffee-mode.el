
(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custon ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
  '(lambda () (coffee-custom)))

(provide 'beyeran-coffee-mode)
