
;;
;; file: emacs.el
;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diverse macros and often used functions                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro multiple-global-set-key (key-pairs)
  "Setting multiple global key at once, e.g.:
       (multiple-key-set ((\"\\C-cx\" first-global)
                          (\"\\M-ax\" second-global)))"
  `(progn
     ,@(loop for item in key-pairs
             collect `(global-set-key ,(first item)
                                      ',(second item)))))

(defmacro require-beyeran (name)
  "Macro for loading special beyeran extensions"
  (let ((require-symbol (intern (concat "beyeran-" name))))
    `(require ',require-symbol)))

(defmacro multiple-add-to-hook (hook forms)
  "Setting multiple variables/functions to a mode-hook, e.g.:
       (multiple-add-to-hook haskell-mode-hook ('turn-on-haskell-doc
                                                'turn-on-haskell-mode))"
  `(progn
     ,@(loop for item in forms
             collect `(add-hook ',hook ,item))))

(defun flatten (x)
  (cond ((null x) nil)
        ((listp x) (append (flatten (car x))
                           (flatten (cdr x))))
        (t (list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting various paths                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possible differences between paths
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))
      beyeran-dir (concat dotfiles-dir "beyeran/")
     windows-home-dir "c:/cygwin/home/yog-soggoth/.emacs.d/"
     extensions-dir (sys-diversification (concat dotfiles-dir     "extensions/")
                                         (concat dotfiles-dir     "extensions/")
                                         (concat windows-home-dir "extensions/"))
      color-theme-dir (concat dotfiles-dir "custom-color-themes/")
      beyeran-color-theme-dir (concat color-theme-dir "beyeran/")
     solarized-color-theme-dir (concat color-theme-dir "solarized/"))

(sys-diversification
    (setq explicit-shell-file-name "/bin/zsh"
           shell-file-name "zsh")
    (setq explicit-shell-file-name "/bin/zsh"
           shell-file-name "zsh")
    (setq explicit-shell-file-name "bash"
          shell-file-name "bash"))

(defun add-to-loadpath (name)
  (add-to-list 'load-path (concat extensions-dir name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path beyeran-dir)
(add-to-list 'load-path extensions-dir)

(add-to-list 'custom-theme-load-path beyeran-color-theme-dir)
(add-to-list 'custom-theme-load-path solarized-color-theme-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading things                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-multiple (folder-list)
  (mapcar #'(lambda (n)
              (add-to-loadpath n)) folder-list))

(setq *extension-list* '("color-theme" 
                         "paredit"
                         "org-mode"
                         "org-mode/lisp"
                         "ido"
                         "rainbow"
                         "ruby-mode"
                         "haskell-mode"
                         "haskell-indent"
                         "clojure-mode"
                         "swank-clojure"
                         "prolog"
                         "scheme"
                         "twittering-mode"
                         "scss-mode"
                         "smex"
                         "ess"
                         "ess/lisp"
                         "coffee-mode"
                         "markdown"
                         "shen-mode"
                         "maxima"
                         "stumpwm"
                         "solarized"
                         "python-mode2"
                         "powerline"
                         "java"))

(sys-diversification
 (append *extension-list* '("/usr/share/emacs/site-lisp/slime/")
 ()
 (load "C:/Users/yog-soggoth/quicklisp/slime-helper.el")))

(load-multiple *extension-list*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require local modifications                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq linum-format "%4d \u2502 ")

;;;;;;;;;; org mode ;;;;;;;;;;
(require-beyeran "org")
(require-beyeran "org-babel")
(require-beyeran "org-export-templates")
;; (require-beyeran "zettel")
(require-beyeran "gtd")

;;;;;;;;;; programming modes ;;;;;;;;;;
(require-beyeran "slime")
(require-beyeran "ruby-mode")
(require-beyeran "haskell-mode")
(require-beyeran "clojure-mode")
(require-beyeran "prolog-mode")
;; (require-beyeran "shen-mode")
;; (require-beyeran "scheme-mode")
;; (require-beyeran "scss-mode")
;; (require-beyeran "coffee-mode")

;;;;;;;;;; math statistics ;;;;;;;;;;
;; (require-beyeran "ess")
;; (require-beyeran "maxima")

;;;;;;;;;; usability ;;;;;;;;;;
(require-beyeran "misc")
(require-beyeran "auto-insert")
(require-beyeran "paredit")
(require-beyeran "smex")
(require-beyeran "eyecandy")

;;;;;;;;;; diverse other modes ;;;;;;;;;;
(require-beyeran "markdown")
