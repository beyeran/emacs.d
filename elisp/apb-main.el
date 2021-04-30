;; General Settings

;;   Setting up the paths so emacs can find all the relvant
;;   configurations.


(defconst apb/emacs-directory (expand-file-name ".emacs.d" (getenv "HOME")))

(defconst apb/emacs-elisp-directory (expand-file-name "elisp" apb/emacs-directory))
(defconst apb/emacs-lisp-directory (expand-file-name "lisp" apb/emacs-directory))

(add-to-list 'load-path apb/emacs-lisp-directory)
(add-to-list 'load-path apb/emacs-elisp-directory)

(setq x-stretch-cursor t
      column-number-mode 1)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(global-hl-line-mode)

(defun apb/emacs-subdiretory (d)
  (expand-file-name d apb/emacs-directory))

;; For seme reason this one got disabled:
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; Just in case...
(global-set-key (kbd "M-s") '(lambda () (interactive) (insert "ß")))
(global-set-key (kbd "M-a") '(lambda () (interactive) (insert "ä")))
(global-set-key (kbd "M-o") '(lambda () (interactive) (insert "ö")))
(global-set-key (kbd "M-u") '(lambda () (interactive) (insert "ü")))



;; Loading helper library:


(require 'apb-helpers)

;; Own Definitions


(require 'cl)

(defmacro def-on-system (name type)
  `(defun ,(intern (concatenate 'string "on-" (symbol-name name))) (&rest @body)
     (when (equal system-type ,type)
       (progn @body))))

(def-on-system win 'windows-nt)
(def-on-system linux 'gnu/linux)
(def-on-system mac 'darwin)

;; Best option in Emacs 23 is to revert to Emacs 22 settings (alt is alt, cmd is meta) with this snippet:
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Package Manager


(require 'package)

(setq package-archives '(("org"      . "http://orgmode.org/elpa/")
			 ("gnu"      . "http://elpa.gnu.org/packages/")
			 ("melpa"    . "http://melpa.org/packages/")))

(package-initialize)

(when (window-system)
  (package-refresh-contents))

;; Use-Package

;;    Using [[https://github.com/jwiegley/use-package][use-package]] to automatically install certain packages, as
;;    well as the ease of lazily loading them.


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Straight integration for use-package


;; Copied verbatim from the repo site.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration with use-package
(use-package el-patch
  :straight t
  :ensure t)

(straight-use-package 'el-patch)

;; Init File Support

;;    Load up a collection of enhancements to Emacs Lisp, including [[https://github.com/magnars/dash.el][dash]],
;;    [[https://github.com/magnars/s.el][s]] for string manipulation, and [[https://github.com/rejeep/f.el][f]] for file manipulation.


(require 'cl)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; Tabs vs. Spaces

;;    Everybody hates tabs in their source code!


(setq-default indent-tabs-mode nil)
(setq tab-width 4)



;; Make tab key do indent first then comlete.


(setq-default tab-always-ident 'complete)

;; Misc Variable Settings

;;    Abbreviate yes-or-no:

(fset 'yes-or-no-p 'y-or-n-p)



;; Keep the point in center while scolling:

(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; Display Settings


(setq-default
 ;; no beeping and no blinking please
 ring-bell-function #'ignore
 visible-bell nil

 ;; make sure that trash is not drawed
 indicate-buffer-boundaries nil
 indicate-empty-lines nil

 ;; don't resize emacs in steps, it looks weird and plays bad with
 ;; window manager.
 window-resize-pixelwise t
 frame-resize-pixelwise t

 ;; disable bidirectional text for tiny performance boost
 bidi-display-reordering nil

 ;; do not highlight regions in non-selected windows
 highlight-nonselected-windows nil

 ;; hide curosrs in other windoes
 cursor-in-non-selected-windows nil

 ;; adjust startup screen
 inhibit-startup-screen t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Undo and Redo

;;    According to [[http://ergoemacs.org/emacs/emacs_best_redo_mode.html][this article]], I get better functionality than
;;    the =redo+= plugin (which I can't seem to get working well).


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Better Jumping

;;    Mostly using the [[https://github.com/abo-abo/avy][avy]] project's [[help:avy-goto-word-timer][avy-goto-word-1]] function, so I bind
;;    that to =C-c j=, but the recent update to include a timer feature,
;;    seems awful sweet:


(use-package avy
  :ensure t
  :bind (("M-n" . 'avy-goto-char-timer))
  :init (setq avy-background t))

;; Tramp


(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-verbose 9)

;; Direx

;;    The [[https://github.com/m2ym/direx-el][direx]] package is a tree-based variation of dired, and it gives
;;    an /ide-like/ look and feel. Not sure of its useful-ness.


;; legacy - heavy todo
;; look at sidebar.el
(require 'dired)

(setq dired-listing-switches "-lah")
(setq dired-hide-details-mode 1)

(use-package dired-subtree
  :ensure t
  :init
  (define-key dired-mode-map (kbd "C-l") 'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "C-j") 'dired-subtree-toggle))

;; SMEX

;;    Built using [[*IDO%20(Interactively%20DO%20Things)][IDO]] to do something similar but with =M-x= commands:


(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
        ("M-X" . smex-major-mode-commands))

;; Helm

;;    Helm


(use-package helm
  :ensure t
  :init
  (use-package helm-config)
  (use-package helm-files))

(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "g") 'helm-do-grep)
(define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)
(define-key helm-map (kbd "M-k") 'helm-next-line)
(define-key helm-map (kbd "M-i") 'helm-previous-line)
(define-key helm-map (kbd "M-v") 'yank)
(define-key helm-find-files-map (kbd "M-k") 'helm-next-line)
(define-key helm-find-files-map (kbd "M-i") 'helm-previous-line)
(define-key helm-find-files-map (kbd "M-v") 'yank)
;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(setq projectile-completion-system 'helm)
(helm-mode 1)

(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(define-key global-map (kbd "C-x b") 'helm-for-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)


;; Display helm buffers always at the bottom
;; Source: http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-reuse-window display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

;; Exec Path From Shell


(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Org

;;    The main org-mode configuration is somewhere else:


(require 'apb-org)

;; Smartparens

;;    Smartparens


(use-package smartparens-config
  :ensure smartparens
  :bind
  (("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("C-M-d" . delete-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)

   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c _"  . wrap-with-underscores)
   ("C-c `"  . wrap-with-back-quotes))
  :init
  (progn
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'latex-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'org-mode-hook 'turn-on-smartparens-strict-mode)
    (show-smartparens-global-mode t)))

;; Company


(use-package company
  :if window-system
  :ensure t
  :init
  (setq company-dabbrev-ignore-case t
        company-show-numbers t)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay nil
        compani-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 20
        company-dabbrev-downcase nil)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-gtags)
  (add-to-list 'company-backends 'company-capf)
  :bind ("C-:" . company-complete)  ; In case I don't want to wait
  :diminish company-mode)

(use-package company-quickhelp
  :if window-system
  :ensure t
  :config
  (company-quickhelp-mode 1))

;; Selecting a Buffer


(use-package kpm-list
  :ensure t
  :bind ("C-x C-b" . kpm-list))

;; Yasnippet & Auto-Complete


(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  (yas-reload-all)
  (setq yas-triggers-in-field t
        org-src-tab-acts-natively t
        org-src-fontify-natively t)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :bind ("<M-tab>" . yas-expand)
  :config
  (add-to-list 'yas-snippet-dirs (apb/emacs-subdiretory "snippets")))

(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dicts")
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  :hook 'prog-mode-hook)

;; Magit


(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :config
  (setq magit-branch-arguments nil
        magit-completing-read-function 'magit-ido-completing-read
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        magit-restore-window-configuration t)
  :bind ("C-x g" . magit-status))

;; Powerline


(use-package powerline
  :ensure t
  :init (powerline-default-theme))

;; Global HS mode


(setq ad-redefinition-action 'accept)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq-default show-trailing-whitespace t)

;; Beacon


(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; Unit Test Mode


(use-package unit-test-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.test\\'" . unit-test-mode))
  (add-hook 'unit-test-mode-hook 'turn-on-smartparens-strict-mode))

;; JSGF Mode

(require 'jsgf-mode)

(add-to-list 'auto-mode-alist '("\\.jsgf\\'" . jsgf-mode))

;; FAS Mode


(use-package fas-mode
  :mode ("\\.fieldannotatedstring\\'" . fas-mode)
  :config
  (define-key fas-mode-map (kbd "C-c a") 'fas/prepare-string-by-fromer-line)
  (define-key fas-mode-map (kbd "C-c d") 'fas/replace-mention-by-sform))

;; Programming Mode Initialization


;; Elisp
(require 'apb-elisp)

;; Python
(require 'apb-python)

;; Org-Mode
(require 'apb-org)

;; C
(require 'apb-c)

;; Haskell
(require 'apb-haskell)

;; Rust
(require 'apb-rust)

;; Eyecandy

;;   Only those which don't rely on a running graphical user interface
;;   (e.g. work when being used from the console).


(use-package tron-legacy-theme
  :ensure t
  :init
  (setq tron-legacy-vivid-curser t)
  (load-theme 'tron-legacy t))

(use-package flycheck-color-mode-line
  :after flycheck
  :ensure t
  :init
  (setq flycheck-color-mode-line-show-running nil)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))



;; And the other stuff:


(when window-system
  (add-to-list 'default-frame-alist
               '(font . "Roboto Mono-13"))
  (set-frame-font "Roboto Mono-13" t t))

(when window-system
  (use-package all-the-icons
    :ensure t)

  (use-package all-the-icons-dired
    :ensure t
    :after all-the-icons
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

;; Server Mode


(when (window-system)
  (setq server-port 42) ;; it had to be this number.

  (server-start))

;; Closing


(provide 'apb-main)
