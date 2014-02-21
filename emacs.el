
(ido-mode)
(require 'eldoc)

(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

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

(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))

(add-to-load-path "~/.emacs.d/src/")
