;; Configuration
  
;;   This is the configuration for the repulsive =Groovy= language, we are
;;   forced to use.


(use-package groovy-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\.stark$"  . groovy-mode))

  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode)))

(provide 'apb-groovy)
