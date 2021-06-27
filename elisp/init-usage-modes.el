;;; init-usage-modes.el --- Configuration for universally used modes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file loads and initializes mostly global modes used uncon-
;; ditional of major mode, like "undo-tree" or "smartparens".  This
;; file is outsorced since it needs to be loaded on startup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 2021/06/27 beyeran
;;    * Added; header2 mode
;;    * Moved: company mode to `init-gui.el`
;; 2021/05/02
;;    * Init
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Integration with use-package
(use-package el-patch
  :straight t
  :ensure t)

(straight-use-package 'el-patch)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; Undo and Redo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)               ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Better Jumping
(use-package avy
  :ensure t
  :bind (("M-n" . 'avy-goto-char-timer))
  :init (setq avy-background t))

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-verbose 9)

;; Direx
(require 'dired)

(setq dired-listing-switches "-lah")
(setq dired-hide-details-mode 1)

(use-package dired-subtree
  :ensure t
  :init
  (define-key dired-mode-map (kbd "C-l") 'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "C-j") 'dired-subtree-toggle))

;; SMEX
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
        ("M-X" . smex-major-mode-commands))

;; Helm
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

(setq helm-split-window-inside-p            t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(defvar projectile-completion-system 'helm)
(helm-mode 1)

(defvar helm-idle-delay 0.1)
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

;; Smartparens
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

;; header2
(add-to-list 'load-path "~/.emacs.d/opt/")
(require 'header2)

(setq header-date-format "%Y/%m/%d")


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
  (setq yas-triggers-in-field t)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :bind ("<M-tab>" . yas-expand)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" apb/emacs-directory)))

(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dicts")
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  :hook 'prog-mode-hook)

;; Global HS mode
(setq ad-redefinition-action 'accept)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(setq-default show-trailing-whitespace t)

;; Beacon
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; CG hack
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(provide 'init-usage-modes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-usage-modes.el ends here
