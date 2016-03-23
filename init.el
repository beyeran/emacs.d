
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar required-packages
  '(;; usability
    magit
    swiper
    powerline
    helm
    helm-company
    helm-ag
    helm-projectile
    smartparens
    cask
    use-package
    projectile
    textmate
    multiple-cursors
    ;; programming modes
    elixir-mode
    alchemist
    web-mode
    lua-mode
    ess
    go-mode
    ;; eyecandy
    railscasts-theme
    org-beautify-theme
    org-bullets))

(require 'cl)

(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  ;; check for new package versions
  (message "%s" "Emacs is now refresing its package database...")
  (package-refresh-contents)
  (message " done.")
  ;; install missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'use-package)

;;
;; helper
;;
(defmacro on-win (&rest body)
  `(when (equal system-type 'windows-nt)
     (progn 
       ,@body)))
  
(defmacro on-linux (&rest body)
  `(when (equal system-type 'gnu/linux)
     (progn 
       ,@body)))

(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))

(setq package-enable-at-startup nil)
(package-initialize)
