
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;;; Variables ;;;;
(setq haskell-program-name "ghci"
      haskell-font-lock-symbols t
      haskell-hoogle-command "hoogle")

(defun custom-haskell-mode ()
  (haskell-indentation-mode -1)
  (haskell-indent-mode 1)
  (flyspell-prog-mode))

;;;; Hooks and Keys ;;;;
(multiple-add-to-hook haskell-mode-hook ('turn-on-haskell-doc-mode
                                         'turn-on-haskell-font-lock
                                         'turn-on-haskell-decl-scan
                                         'custom-haskell-mode
                                         'turn-on-haskell-simple-indent
                                         (lambda ()
                                           (define-keys haskell-mode-map
                                             '(("RET" newline)
                                               ("TAB" haskell-indent-cycle)
                                               ("C-c =" haskell-indent-insert-equal)
                                               ("C-c |" haskell-indent-insert-guard)
                                               ("C-c o" haskell-indent-insert-otherwise)
                                               ("C-c w" haskell-indent-insert-where)
                                               ("C-c ." haskell-indent-align-guards-and-rhs)
                                               ("C-c i" inferior-haskell-info))))))

(add-hook 'inferior-haskell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") 'haskell-hoogle)
            (turn-on-haskell-doc-mode 1)))


;;;; Requirements ;;;;
(require 'haskell-mode "haskell-mode" t)
(require 'inf-haskell "inf-haskell" t)
;;(require 'haskell-indent "haskell-indent" t)

(provide 'beyeran-haskell-mode)
