
;; diversification functions for my both systems,
;; mac and linux
(defun sys-type (name)
  (if (eq system-type name)
      t
    nil))

(defmacro sys-diversification (darwin gnu)
  `(cond ((sys-type 'darwin) ,darwin)
         ((sys-type 'gnu) ,gnu)
         (t nil)))

(defun sys-dependent (darwin gnu)
  (eval (sys-diversification darwin gnu)))

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
                         "clojure-mode"
                         "qi-mode"
                         "clojure-mode"
                         "prolog"
                         "scheme"
                         "ess"
                         "scss-mode"
                         "shen-mode"))

(load-multiple *extension-list*)

(sys-dependent
 '(require 'beyeran-color-theme-mac)
 '(require 'beyeran-color-theme-linux))

(require 'beyeran-slime)
;;(require 'beyeran-haml)
;;(require 'beyeran-magit)
(require 'beyeran-misc)
(require 'beyeran-org)
(require 'beyeran-org-babel)
(require 'beyeran-org-export-templates)
(require 'beyeran-org-reftex)
(require 'beyeran-paredit)
(require 'ido)
(require 'beyeran-ruby-mode)
(require 'beyeran-ess)
(require 'beyeran-haskell-mode)
(require 'beyeran-clojure-mode)
(require 'beyeran-prolog-mode)
(require 'beyeran-shen-mode)
(require 'beyeran-scheme-mode)
(require 'beyeran-scss-mode)
