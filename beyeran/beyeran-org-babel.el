
;;
;; file: beyeran-org-babel.el
;;

;;; fontification ;;;
(setq org-src-fontify-natively t)

;;; org babel ;;;
(require 'ob)
(require 'ob-eval)
(require 'ob-lisp)
(require 'ob-ruby)
(require 'ob-R)

(setq org-src-fontify-natevely t)
(setq org-confirm-babel-evaluate nil)

(setq org-babel-load-languages (quote ((emacs-lisp . t)
                                       (dot . t)
                                       (ditaa . t)
                                       (R . t)
                                       (python . t)
                                       (ruby . t)
                                       (gnuplot . t)
                                       (clojure . t)
                                       (sh . t))))
(setq org-confirm-babel-evaluate nil)

(provide 'beyeran-org-babel)
