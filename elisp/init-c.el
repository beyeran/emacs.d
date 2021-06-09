;; #+TITLE:  C Programming Mode


(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


(use-package disaster
  :config
  (setq disaster-objdump "objdump -d --x86-asm-syntax=intel -Sl --no-show-raw-insn"))

(provide 'init-c)
