;;; init.el --- Main initialization.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Personal configuration for Emacs LISP mode.   As always still under
;; development.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2021-06-06
;;  * Next Redo (e.g. renaming from "elisp/apb-main.el")
;; 2021-05-02
;;  * Major redo.
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
(defconst apb/emacs-directory (expand-file-name ".emacs.d" (getenv "HOME"))
  "Emacs config root directory.")

(defconst apb/emacs-elisp-directory
  (expand-file-name "elisp" apb/emacs-directory)
  "Path to local Emacs confic files.")

(add-to-list 'load-path apb/emacs-elisp-directory)

;; Core
(require 'config-basic)
(require 'init-package-loading)

;; Utilities
(require 'config-gui)
(require 'init-usage-modes)
(require 'init-ide)

;; Languages
(require 'init-c)
(require 'init-elisp)
(require 'init-haskell)
(require 'init-python)
(require 'init-rust)

;; General Purpose / Learning
(require 'init-org)
(require 'config-org)
(require 'init-notes)
(require 'init-writing)
(require 'init-flashcards)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
