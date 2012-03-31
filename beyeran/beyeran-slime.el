
(sys-diversification
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  ())

(sys-diversification
  (setq inferior-lisp-program "/Applications/CCL/dx86cl")
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(require 'slime "slime" t)

(slime-setup '(slime-fancy slime-asdf slime-references slime-indentation))

(setq slime-enable-evaluate-in-emacs t
      slime-net-coding-system 'utf-8-unix
 ;;     lisp-indent-function 'cl-indent:function 
 )

;;(setq slime-lisp-implementations
;;      `((sbcl ,@(list (sys-diversification "/opt/local/bin/sbcl " "/usr/bin/sbcl ")))
;;       (clisp ("clisp" "-E utf-8" "-modern"))
;;        )
;;      slime-default-lisp 'sbcl)

(add-hook 'slime-mode-hook
          (lambda ()
            (define-keys slime-mode-map
                '(("C-c s" slime-selector)
                  ("C-j" newline-and-indent)
                  ("TAB" slime-indent-and-complete-symbol)
                  ("C-c C-d c" cltl2-lookup)))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-keys slime-repl-mode-map
                '(("C-c s" slime-selector)
                  ("C-c C-d c" cltl2-lookup)))))

(provide 'beyeran-slime)
