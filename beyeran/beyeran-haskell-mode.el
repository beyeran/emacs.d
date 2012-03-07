
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;;; Variables ;;;;
(setq haskell-program-name "ghci"
      haskell-font-lock-symbols t)

(custom-set-variables '(indent-tabs-mode nil))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;;;; Hooks and Keys ;;;;
(setq haskell-hoogle-command "hoogle")

(defun add-haskell-hooks ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 
            (lambda ()
              (define-keys haskell-mode-map
                '(("RET" newline)
                  ("TAB" haskell-indent-cycle)
                  ("C-c =" haskell-indent-insert-equal)
                  ("C-c |" haskell-indent-insert-guard)
                  ("C-c o" haskell-indent-insert-otherwise)
                  ("C-c w" haskell-indent-insert-where)
                  ("C-c ." haskell-indent-align-guards-and-rhs)
                  ("C-c i" inferior-haskell-info)))))
  (add-hook 'inferior-haskell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c h") 'haskell-hoogle)
              (turn-on-haskell-doc-mode 1))))


;;;; Requirements ;;;;
(when (and (require 'haskell-mode "haskell-mode" t)
           (require 'inf-haskell "inf-haskell" t)
       (require 'haskell-indent "haskell-indent" t))
  (add-haskell-hooks))

(provide 'beyeran-haskell-mode)
