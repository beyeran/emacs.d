;; Basic Mode


(use-package haskell-mode
  :ensure t)

;; Hasklig


(use-package hasklig-mode
  :ensure t
  :config
  (set-face-attribute 'default nil
                      :family "Hasklig"
                      :height 150
                      :weight 'normal
                      :width 'normal)
  :hook (haskell-mode))

;; Technical Artifacts

;;   Make sure that we can simply =require= this library.


(provide 'apb-haskell)
