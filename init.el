(require 'package)

(defun add-package-archive (entry)
  (add-to-list 'package-archives entry t))

(defun add-package-archives (archive-list)
  (mapcar 'add-package-archive archive-list))

(package-initialize)

(add-package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                        ("org" . "http://orgmode.org/elpa/")))

(require 'cl)

(setq required-packages
      '(;; org
        org-plus-contrib
        ;; usability
        undo-tree
        magit
        swiper
        ;; powerline
        rainbow-delimiters
        smartparens
        cask
        use-package
        projectile
        textmate
        multiple-cursors
        git-gutter-fringe+
        bookmark+
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
        ensime
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
        ;; highlight-indentation
        ;; indent-guide
        twilight-bright-theme
        soothe-theme
        noctilux-theme
        moe-theme
        mellow-theme
        monokai-theme
        exec-path-from-shell
        org-beautify-theme
        org-bullets))

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
(defmacro def-on-system (name type)
  `(defun ,(intern (concatenate 'string "on-" (symbol-name name))) (&rest @body)
     (when (equal system-type ,type)
       (progn @body))))

(def-on-system win 'windows-nt)
(def-on-system linux 'gnu/linux)
(def-on-system mac 'darwin)

(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))

(setq package-enable-at-startup nil)
(package-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
