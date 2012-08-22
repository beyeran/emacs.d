
;;
;; file: emacs.el
;;


;; some macros
(defmacro sys-diversification (gnu/linux &optional darwin win)
  "System diversification for Linux, Mac and Windows, focus on Linux"
  `(cond ((sys-type 'darwin) ,darwin)
         ((sys-type 'gnu/linux) ,gnu/linux)
         ((sys-type 'windows-nt) ,win)
         (t nil)))

(defun sys-type (name)
  (if (eq system-type name)
      t
    nil))

(defmacro require-beyeran (name)
  (let ((require-symbol (intern (concat "beyeran-" name))))
    `(require ',require-symbol)))

(defun flatten (x)
  (cond ((null x) nil)
        ((listp x) (append (flatten (car x))
                           (flatten (cdr x))))
        (t (list x))))

;; possible differences between paths
(setq explicit-shell-file-name "/bin/zsh")
(setq shell-file-name "zsh")
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; functions for loading the right folder
(setq dotfiles-dir (file-name-directory
  (or (buffer-file-name) load-file-name)))

(setq beyeran-dir (concat dotfiles-dir "beyeran/"))

(setq extensions-dir (concat dotfiles-dir "extensions/"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path beyeran-dir)
(add-to-list 'load-path extensions-dir)

(defun add-to-loadpath (name)
  (add-to-list 'load-path (concat extensions-dir name)))

(defun load-multiple (folder-list)
  (mapcar #'(lambda (n)
              (add-to-loadpath n)) folder-list))

(setq *extension-list* '("color-theme" 
                         "paredit"
                         "org-mode"
                         "org-mode/lisp"
                         "ido"
                         "ruby-mode"
                         "haskell-mode"
                         "haskell-indent"
                         "clojure-mode"
                         "swank-clojure"
                         "prolog"
                         "scheme"
                         "scss-mode"
                         "smex"
                         "ess"
                         "ess/lisp"
                         "coffee-mode"
                         "shen-mode"
                         "maxima"
                         "java"))

(sys-diversification
 (append *extension-list* '("/usr/share/emacs/site-lisp/slime/")))

(load-multiple *extension-list*)

;; requiring local files

(require-beyeran "misc")
(require-beyeran "auto-insert")
(require-beyeran "paredit")
(require-beyeran "smex")
(require-beyeran "org")
(require-beyeran "org-babel")
(require-beyeran "org-reftex")
(require-beyeran "org-export-templates")
(require-beyeran "jekyll")
(require-beyeran "ruby-mode")
(require-beyeran "haskell-mode")
(require-beyeran "clojure-mode")
(require-beyeran "prolog-mode")
;(require-beyeran "shen-mode")
;;(require-beyeran "scheme-mode")
;(require-beyeran "scss-mode")
;(require-beyeran "coffee-mode")
(require-beyeran "ess")
(require-beyeran "gtd")
;(require-beyeran "maxima")
;(require-beyeran "java")
(require-beyeran "octave")
;;(require-beyeran "erlang-mode")
(require-beyeran "slime")
