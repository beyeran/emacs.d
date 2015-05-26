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

;; (add-hook 'lisp-mode-hook (prettify-symbols-mode))
;; (add-hook 'emacs-lisp-mode-hook (prettify-symbols-mode))

(case system-type
  (windows-nt
   (setenv "CYGWIN" (concat (getenv "CYGWIN") " nodosfilewarning"))
   (mapc (apply-partially 'add-to-list 'exec-path)
         `("C:/Perl/bin"
           "C:/cygwin/bin"))
   (setenv "PATH"
           (mapconcat 'identity
                      `(
                        "C:/cygwin/bin"
                        "C:/MinGW/bin"
                        ,(getenv "PATH"))
                      ";")))
  (gnu/linux
   (setenv "LC_MESSAGES" "C")
   (setenv "MANWIDTH" "72")))
