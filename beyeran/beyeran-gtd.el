
;;
;; file: beyeran-gtd.el
;;

(setq calendar-date-style 'iso
      diary-show-holidays-flag nil
      calendar-week-start-day 1
      calendar-mark-diary-entries-flag t)

;; general gtd path
(setq org-directory
      (sys-diversification (expand-file-name "~/projects/gtd")
                           (expand-file-name "~/Projects/gtd")))

;; paths as described:
(setq *gtd-paths* '(("general" ("todo.org"
                                "systems-theory.org"
                                "website.org"
                                "finances.org"))
                    ("term-paper" ("term-paper-risk.org"
                                   "term-paper-game-theory.org"
                                   "term-paper-qa.org"))
                    ("revision" ("statistics-revision.org"))
                    ("university-work" ("motifs-research.org"
                                        "hiwi-soz.org"))))

;; functions to make paths available
(defun append-folder-with-files (folder-file-structure)
  "Appends one folder with it's containing files. FOLDER-FILE-STRUCTURE
is the folder's name which contains the files to associate"
  (mapcar #'(lambda (entry) (format "%s/%s/%s" org-directory (first folder-file-structure) entry))
          (first (rest folder-file-structure))))

(defun append-folders (path-structure)
  "Generates a list of path names from a PATH-STRUCTURE. See *gtd-paths* 
for the structure"
  (flatten (mapcar #'append-folder-with-files path-structure)))

;; setting structure to agenda list
(setq org-agenda-files (append-folders *gtd-paths*))
;; added refill
;;(setq org-agenda-files (append org-agenda-files
;;                               (list (format "%s%s" org-directory "refill.org"))))

;; key settings (testing)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c r") 'org-capture)

(global-set-key (kbd "C-M-h") 'bh/hide-other)
(global-set-key (kbd "C-M-c") 'org-cycle-agenda-files)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

;; todo customization
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD"))
        (done ("WAITING") ("CANCELLED") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

(setq org-default-notes-file (format "%s%s" org-directory "refill.org"))

(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("n" "note" entry (file org-default-notes-file)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("h" "habit" entry (file org-defalut-notes-file)
         "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("i" "issue" entry (file org-default-notes-file)
         "* TODO %? :ISSUE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("b" "books" entry (file org-default-notes-file)
         "* TODO %? :BOOK:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("p" "paper" entry (file org-default-notes-file)
         "* TODO %? :PAPER:\n%U\n%a\n" :clock-in t :clock-resume t)))

;; using ido to manage refill.org notes and move them to the files they needet to be in
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; use ido for both buffer and file completion and ido-everywhere
(setq org-completion-use-ido t)
(setq ido-everywere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; task treatment
(setq org-use-fast-todo-selection t) ;; C-c C-t KEY -> defined in org-todo-keywords

(provide 'beyeran-gtd)
