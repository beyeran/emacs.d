
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(setq required-packages
  '(magit paredit ess helm elixir-mode web-mode lua-mode swiper powerline alchemist
          badwolf-theme helm-company use-package org-beautify-theme
          org-bullets cask))

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

(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))
