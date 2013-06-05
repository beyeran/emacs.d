
(setq clojure-src-root (expand-file-name "~/.emacs.d/extensions"))

(autoload 'clojure-mode "clojure-mode" t)
(autoload 'clojure-test-mode "clojure-test-mode" nil t)

(progn
  (autoload 'swank-clojure-init "swank-clojure")
  (autoload 'swank-clojure-slime-mode-hook "swank-clojure")
  (autoload 'swank-clojure-cmd "swank-clojure")
  (autoload 'swank-clojure-project "swank-clojure"))

;; Java starves programs by default
(setq swank-clojure-extra-vm-args (list "-Xmx1024m"))

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(provide 'beyeran-clojure-mode)
