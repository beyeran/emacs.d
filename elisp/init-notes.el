;;; init-notes.el --- Note organization.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Roam
(defvar *roam-directory*
  (expand-file-name "~/projects/zettelkasten")
  "Local directory for storing roam related files.")


(defvar *apb/concept-note-descriptor*
  "concept"
  "String of the identifer used as concept note directory.")

(defvar *apb/concept-directory*
  (expand-file-name *apb/concept-note-descriptor* *roam-directory*)
  "Directory for storing concept notes.")

(unless (file-directory-p *apb/concept-directory*)
  (make-directory *apb/concept-directory*))


(defvar *apb/fleeting-note-descriptor*
  "fleeting"
  "String of the identifer used as concept note directory.")

(defvar *apb/fleeting-directory*
  (expand-file-name *apb/fleeting-note-descriptor* *roam-directory*)
  "Directory for storing fleeting notes.")

(unless (file-directory-p *apb/fleeting-directory*)
  (make-directory *apb/fleeting-directory*))


(defvar *apb/literature-note-descriptor*
  "literature"
  "String of the identifer used as concept note directory.")

(defvar *apb/literature-directory*
  (expand-file-name *apb/literature-note-descriptor* *roam-directory*)
  "Directory for storing literature notes,  a note with a referance in the bibliographic file.")

(defvar *apb/bibliographic-file*
  (expand-file-name "bibliography.bib" *apb/literature-directory*)
  "Path to the bibliographic file - pre-requisit for literature notes.")

(unless (file-directory-p *apb/literature-directory*)
  (make-directory *apb/literature-directory*))


(defun apb/file-in-org-roam-directory-p (path)
  "Check if a given PATH is inside the `*roam-directory*'."
  (interactive)
  (string-prefix-p *roam-directory* (file-name-directory path)))


(use-package org-roam
  :ensure t
  :straight (:host github :repo "org-roam/org-roam"
                   :branch "v2")
  :hook
  ((after-init . org-roam-mode)
   (org-mode . (lambda () (org-babel-execute-buffer))))
  :init
  (when (not (file-directory-p *roam-directory*))
    (make-directory *roam-directory*))
  (on-mac
   (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox"))
  :config
  (require 'org-roam-protocol)

  (setq org-confirm-babel-evaluate nil
        org-roam-index-file "index.org"
        org-roam-directory *roam-directory*
        org-startup-with-latex-preview t)

  :bind (:map org-roam-mode-map
              (("C-c n i" . org-roam-insert)
               ("C-c n t" . org-roam-tag-add)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer))))


(use-package org-ref
  :after (org org-roam)
  :ensure t
  :config
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-default-bibliography (list *apb/bibliographic-file*)
        bibtex-completion-bibliography *apb/bibliographic-file*
        org-ref-note-title-format (concat "* NOTES %y - %t\n "
                                          ":PROPERTIES:\n  "
                                          ":Custom_ID: %k\n  "
                                          ":NOTER_DOCUMENT: %F\n "
                                          ":ROAM_KEY: cite:%k\n  "
                                          ":AUTHOR: %9a\n  "
                                          ":JOURNAL: %j\n  "
                                          ":YEAR: %y\n  "
                                          ":VOLUME: %v\n  "
                                          ":PAGES: %p\n  "
                                          ":DOI: %D\n  "
                                          ":URL: %U\n "
                                          ":END:\n\n")
        org-ref-notes-directory *apb/literature-directory*
        org-ref-notes-function 'orb-edit-notes))


(use-package org-roam-bibtex
  :after (org org-roam)
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n a" . orb-note-actions))
  :config
  (setq orb-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor" "keywords")
        orb-templates '(("r" "ref" plain (function org-roam-capture--get-point) ""
                         :file-name "literature/${slug}"
                         :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :AUTHOR: ${author-or-editor}
  :END:

"
                         :unnarrowed t))))


(use-package company-org-roam
  :ensure t
  :after (org org-roam)
  :straight (:host github :repo "org-roam/company-org-roam")
  :config
  (push 'company-org-roam company-backends)
  (setq org-roam-completion-everywhere t)
  :bind (("C-n" . company-select-next)
         ("C-t" . company-select-previous)))


(use-package deft
  :ensure t
  :after (org org-roam)
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension)
  (deft-directory *roam-directory*))


(provide 'init-notes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-notes.el ends here
