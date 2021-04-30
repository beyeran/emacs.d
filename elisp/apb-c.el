;; #+TITLE:  C Programming Mode


(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))




;; Make sure that we can simply =require= this library.


(provide 'apb-c)
