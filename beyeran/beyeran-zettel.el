
;;
;; file: beyeran-zettel.el
;;

;; general variables
(require 'remember)

(org-remember-insinuate)

(defvar *note-file* "notes.org")
(defvar *note-directory* "~/documents/org/")
(defvar *bibliographic-file* "zettel.bib")

(setq org-directory *note-directory*
          org-default-note-file (concat org-directory *note-file*)
          org-agenda-include-diary t
          org-use-fast-todo-selection t)

;; getting bibliographic information
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
                 ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
                 (global-auto-revert-mode t)
                 (reftex-parse-all)
                 ;; add a custom reftex cite format to insert links
                 (reftex-set-cite-format '((?\C-m "\[cite][%l]"))))))

  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search)

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[%s]]" (reftex-citation t))))

(setq reftex-default-bibliography (list (format "%s%s" 
                                                                                                *note-directory*
                                                                                                *bibliographic-file*)))

(setq org-link-abbrev-alist
      '(("bib" . (format "%s%s" *bibliographic-file* "::%s"))))

(setq org-capture-templates '(("z" "Zettel" entry (file org-default-note-file)
                                                           "* %^{title} \t %^g \n  :CITATION: %(reftex-citation) \n  :PAGE: %^{page}\n\n  %?")))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(multiple-global-set-key (("\C-cr" org-capture)))

(provide 'beyeran-zettel)
