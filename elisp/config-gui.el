;;; config-gui.el -- Configuration for the editor's general look.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2021-06-08
;;  * Theme Change.
;; 2021-06-06
;;  * Renaming
;; 2021-05-03
;;  * Changed theme
;; 2021-05-02
;;  * Initial commit.
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
(setq default-frame-alist
      '((font                  . "Roboto Mono:style=Light:size=14")
        (min-height            . 1)
        (height                . 45)
        (min-width             . 1)
        (width                 . 81)
        (vertical-scroll-bars  . nil)
        (internal-border-width . 24)
        (left-fringe           . 0)
        (right-fringe          . 0)
        (tool-bar-lines        . 0)
        (menu-bar-lines        . 0)))

;; Fall back font for glyph missing in Roboto
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
          (lambda () (setq buffer-display-table (make-display-table))))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)

(setq x-underline-at-descent-line t)

;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; Color theme
(use-package almost-mono-themes
  :ensure t
  :init
  (load-theme 'almost-mono-black t))

(setq custom-safe-themes t)

;; Icons
(when window-system
  (use-package all-the-icons
    :ensure t)

  (use-package all-the-icons-dired
    :ensure t
    :after all-the-icons
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(provide 'config-gui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config-gui.el ends here
