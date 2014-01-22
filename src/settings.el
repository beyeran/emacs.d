
(put 'with-module 'lisp-indent-function 1)

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(lambda\\)\\>" (0 (prog1 ()
                                              (compose-region (match-beginning 1)
                                                              (match-end 1)
                                                              λ))))))

(font-lock-add-keywords 'python
                        '(("lambda" (0 (prog1 ()
                                         (compose-region (match-beginning 1)
                                                         (match-end 1)
                                                         λ))))))
