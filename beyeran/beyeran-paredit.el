
;;
;; file: beyeran-paredit.el
;;

;;;;;;;; paredit ;;;;;;;;
(require 'paredit)

(when (require 'paredit "paredit" t)
  (mapc (lambda (hook) (add-hook hook (lambda () (paredit-mode 1))))
        '(emacs-lisp-mode-hook
          lisp-mode-hook
          slime-repl-mode-hook
          slime-mode-hook
          inferior-qi-mode-hook
          qi-mode-hook
          scheme-mode
          clojure-mode-hook)))

(provide 'beyeran-paredit)
