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

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

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
