;; #+TITLE:  Rust Programming Mode


(use-package rust-mode
  :ensure t
  :bind (("C-c <tab>" . rust-format-buffer)))

(use-package cargo
  :ensure t
  :hook rust-mode)

(use-package flycheck-rust
  :ensure t
  :hook flycheck-mode)

(provide 'init-rust)
