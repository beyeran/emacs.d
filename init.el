(require 'package)

(defun add-package-archive (entry)
  (add-to-list 'package-archives entry t))

(defun add-package-archives (archive-list)
  (mapcar 'add-package-archive archive-list))

(add-package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(setq required-packages
      '(;; org
        org-plus-contrib
        ;; usability
        magit
        swiper
        powerline
        smartparens
        cask
        use-package
        projectile
        textmate
        multiple-cursors
        git-gutter-fringe+
        ;; helm
        helm
        helm-company
        helm-ag
        helm-projectile
        ;; templating
        yasnippet
        elixir-yasnippets
        elm-yasnippets
        ;; programming modes
        elixir-mode
        alchemist
        web-mode
        lua-mode
        php-mode
        ess
        go-mode
        inf-ruby
        flycheck
        clojure-mode
        cider
        flycheck-clojure
        elm-mode
        ;; eyecandy
        highlight-indentation
        indent-guide
        mellow-theme
        exec-path-from-shell
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

(defmacro on-mac (&rest body)
  `(when (equal system-type 'darwin)
     (progn
       ,@body)))

(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))

(setq package-enable-at-startup nil)
(package-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
