;; Roam
(defvar *roam-directory* "~/projects/braindump/org"
  "Local directory for storing roam related files.")

(defun apb/org-auto-load-hook ()
  (org-babel-execute-buffer))

(defvar *apb/literature-directory*
  (expand-file-name "literature" *roam-directory*)
  "Directory for storing literature notes. A literature note it a note with a referance in the bibliographic file.")

(unless (file-directory-p *apb/literature-directory*)
  (make-directory *apb/literature-directory*))

(defvar *apb/bibliographic-file*
  (expand-file-name "bibliography.bib" *apb/literature-directory*)
  "Path to the bibliographic file - prerequisit for literature notes.")

(defun apb/org-id-update-org-roam-files ()
  "Update Org-ID locations for all Org-roam files."
  (interactive)
  (org-id-update-id-locations (org-roam--list-all-files)))

(defun apb/org-id-update-id-current-file ()
  "Scan the current buffer for Org-ID locations and update  them."
  (interactive)
  (org-id-update-id-locations (list (buffer-file-name (current-buffer)))))

(use-package org-roam
  :ensure t
  :straight (:host github :repo "org-roam/org-roam")
  :hook
  ((after-init . org-roam-mode)
   (org-mode . apb/org-auto-load-hook))
  :init
  (when (not (file-directory-p *roam-directory*))
    (make-directory *roam-directory*))
  (on-mac
   (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox"))
  :config
  (require 'org-roam-protocol)

  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section)
        org-confirm-babel-evaluate nil
        org-roam-index-file "index.org"
        org-roam-directory *roam-directory*
        org-startup-with-latex-preview t)

  (defun apb/org-roam-insert ()
    "TODO"
    (interactive)
    (let ((description (read-string "Description: ")))
      (org-roam-insert nil nil nil description "id")))

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

(use-package org-roam-server
  :ensure t
  :after (org org-roam)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-files nil
        org-roam-server-served-file-extensions '("pdf")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-lenght 60
        org-roam-server-network-label-wram-length 20))

(defun apb/get-all-org-links-in-file ()
  """TODO"""
  (interactive)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link) (string= (org-element-property :type link) "file")
      (org-element-property :path link))))

(provide 'init-notes)
