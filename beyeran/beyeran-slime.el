
(load (expand-file-name "~/.quicklisp/slime-helper.el"))

(require 'slime "slime" t)

(slime-setup '(slime-fancy slime-asdf slime-references slime-indentation))

(setq slime-enable-evaluate-in-emacs t slime-net-coding-system 'utf-8-unix)

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

(defun clojure-slime-config ()
  (require 'slime-autoloads)
  
  (slime-setup '(slime-fancy))

  (setq swank-clojure-classpath
        (list
         (concat clojure-src-root "/clojure/clojure.jar")
         (concat clojure-src-root "/clojure-contrib/target/clojure-contrib-1.2.0-SNAPSHOT.jar")
         (concat clojure-src-root "/swank-clojure/src")
         (concat clojure-src-root "/clojure/test/clojure/test_clojure")))

  (eval-after-load 'slime
    '(progn (require 'swank-clojure)
            (setq slime-lisp-implementations
                  (cons `(clojure ,(swank-clojure-cmd) :init
                                  swank-clojure-init)
                        (remove-if #'(lambda (x) (eq (car x) 'clojure))
                                   slime-lisp-implementations))))))

;; http://groups.google.com/group/clojure/browse_thread/thread/e70ac373b47d7088 
(add-to-list 'slime-lisp-implementations
             (sys-diversification 
              '(sbcl ("/usr/bin/sbcl"))
              '(ccl ("/Applications/CCL/dx86cl"))))


(defun pre-slime-clj (&optional clj-p)
  "Stuff to do before SLIME runs" 
  (unless (eq clj-p nil)
    (clojure-slime-config))
  (slime-setup '(slime-fancy)))


(defun run-clojure () 
  "Starts clojure in Slime" 
  (interactive)
  (pre-slime-clj t)
  (slime 'clojure))

(defun run-lisp () 
  "Starts SBCL in Slime" 
  (interactive) 
;;  (pre-slime-clj)
  (sys-diversification
   (slime 'sbcl)
   (slime 'ccl)))

(provide 'beyeran-slime)
