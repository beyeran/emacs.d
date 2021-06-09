;;; init-ide.el --- IDE like features -*- lexical-binding: t; -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2021-06-07
;;  * Init
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

(use-package company
  :defer 2
  :diminish
  :hook (after-init . global-company-mode)
  :defines (company-backends)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line
             global-company-mode)
  :init
  (setq-default
   company-minimum-prefix-length 2
   company-tooltip-limit 14
   company-tooltip-align-annotations t
   company-require-match 'never
   company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode)
   company-backends '(company-capf)
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend)))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode)
  :defines (prescient-save-file)
  :commands (prescient-persist-mode)
  :config
  (setq prescient-save-file (concat apb/emacs-directory
                                    "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package flycheck
  :defer 1
  :commands (global-flycheck-mode)
  :init
  (setq-default
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-check-syntax-automatically
   '(save idle-change mode-enabled)
   flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode +1))

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-lens-mode)
  :init
  (setq-default
   lsp-session-file (concat apb/emacs-directory "lsp-session")
   lsp-auto-guess-root nil
   lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(use-package posframe
  :ensure t)

(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package eglot
  :ensure t
  :defines (eglot-server-programs))

(provide 'init-ide)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ide.el ends here
