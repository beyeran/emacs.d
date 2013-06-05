
;;
;; file: beyeran-org-babel.el
;;

;;; org babel ;;;
(require 'ob)
(require 'ob-eval)
(require 'ob-lisp)
(require 'ob-ruby)
(require 'ob-R)
(require 'ob-maxima)

(setq org-src-fontify-natevely t
      org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages 
 '((emacs-lisp . t)
   (dot . t)
   (lisp . t)
   (octave .t)
   (ditaa . t)
   (R . t)
   (python . t)
   (ruby . t)
   (maxima . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)))

(provide 'beyeran-org-babel)
