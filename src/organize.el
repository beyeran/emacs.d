
;;
;; using org mode as organzier
;;
(add-to-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(defvar *todo* "~/planer/todo.org")
(defvar *notes* "~/planer/notes.org")
(defvar *calendar* "~/planer/calendar.org")

(setq org-agenda-files (list *todo* *calendar*))

(setq org-capture-templates
        '(("t" "Task" entry (file+headline *todo* "Inbox")
           "* TODO %? \n  %i" :clock-in t :clock-resume t)
          ("n" "Retailiate" entry (file+headline *todo* "Retailiate")
           "* LOOK %? \n  %i" :clock-in t :clock-resume t)
          ("r" "Read" entry (file+headline *todo* "Read")
           "* READ %? \n  %i" :clock-in t :clock-resume t)
          ("j" "Notes" entry (file+datatree *notes*)
           "* %?\nEntered on %U\n  %i")
          ("d" "Dates" entry (file+datatree *calendar*)
           "* DATE %? \n  %i" :clock-in t :clock-resume t)))

(setq org-hide-leading-stars 'hidestars)
(setq org-return-follows-link t)
(setq org-completion-use-ido t)
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-drawers (quote ("PROPERTIES" "CLOCKTABLE" "LOGBOOK" "CLOCK")))
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "FINISHED(s!)" "LOOK(n)" "SEEN(m!)" 
                  "READ(r)" "DONE(q!)" "DATE(d)" "DELEGATED(c)" "CANCELED(b)")))
(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#b70101" :weight bold))
        ("STARTED"  . (:foreground "#b70101" :weight bold))
        ("LOOK"  . (:foreground "sienna" :weight bold))
        ("SEEN"  . (:foreground "blue" :weight bold))
        ("READ"  . (:foreground "orange" :weight bold))
        ("DATE"  . (:foreground "orange" :weight bold))
        ("DONE"  . (:foreground "forestgreen" :weight bold))
        ("DELEGATED"  . (:foreground "forestgreen" :weight bold))
        ("CANCELED"  . shadow)))

;;
;; agenda
;;
;;(add-hook 'org-agenda-mode-hook '(lambda () (h1-line-mode 1)))
(setq org-agenda-format-date 
 "%Y-%m-%d ---------------------------------------------------------------------")
(setq org-agenda-fontify-priorities 
      '((65 (:foreground "Red")) 
        (66 (:foreground "Blue")) 
        (67 (:foreground "Darkgreen"))))

(setq org-agenda-date-weekend '(:foreground "Yellow" :weight bold))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-span 1)
