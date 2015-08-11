
(require 'eldoc)

(prefer-coding-system 'utf-8)

(setq user-mail-address "beyeran@gmail.com")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(defmacro with-module (symbol name-string &rest body)
  `(condition-case nil
       (progn
         (add-to-load-path  ,(format "%s%s" *modules-dir* name-string))
         (autoload ',symbol ,name-string ,name-string t)
         ,@body)
 
     (error (message (format " => problem loading %s" ',symbol))
            nil)))

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (add-to-load-path ,(format "%s%s" *modules-dir* symbol))
         (require ',symbol)
         ,@body)))

(defun require-special-theme (symbol variant)
  (condition-case nil
      (progn
        (add-to-load-path (format "~/.emacs.d/color-theme/%s" symbol))
        (require (intern (format "%s-theme" symbol)))
        (load-theme (intern (format "%s-%s" symbol variant)) t))
  
    (error (message (format " => problem loading %s" symbol))
            nil)))

(defun add-to-load-path (path)
  "Wrapps the ADD-TO-LIST function for the LOAD-PATH variable"
  (add-to-list 'load-path path))

(defun add-theme (path)
  "Wrapps the ADD-TO-LIST function for the CUSTOM-THEME-LOAD-PATH variable"
  (add-to-list 'custom-theme-load-path (format "~/.emacs.d/color-theme/%s"
                           path)))

(defun add-to-alist (suffix-mode-list)
  "Wrapps the ADD-TO-LIST function for the AUTO-MODE-ALIST variable"
  (add-to-list 'auto-mode-alist suffix-mode-list))

;; remember tangle files before!
(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))

(add-to-load-path "~/.emacs.d/src/")
