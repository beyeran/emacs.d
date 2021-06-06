;;; config-basic.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2021-06-06
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
(defmacro def-on-system (name type)
  `(defun ,(intern (concat "on-" (symbol-name name))) (&rest @body)
     (when (equal system-type ,type)
       (progn @body))))

(def-on-system win 'windows-nt)
(def-on-system linux 'gnu/linux)
(def-on-system mac 'darwin)

(setq custom-file (concat apb/emacs-elisp-directory "custom.el"))

(setq x-stretch-cursor t
      column-number-mode 1)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq default-buffer-file-coding-system 'utf-8)

(global-hl-line-mode)

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

;; For seme reason this one got disabled:
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; Just in case...
(global-set-key (kbd "M-s") '(lambda () (interactive) (insert "ß")))
(global-set-key (kbd "M-a") '(lambda () (interactive) (insert "ä")))
(global-set-key (kbd "M-o") '(lambda () (interactive) (insert "ö")))
(global-set-key (kbd "M-u") '(lambda () (interactive) (insert "ü")))

;; Best option in Emacs 23 is to revert to Emacs 22 settings (alt is alt, cmd is meta) with this snippet:
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Server Mode
(when (window-system)
  (setq server-port 42) ;; it had to be this number.

  (server-start))


(provide 'config-basic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config-basic.el ends here
