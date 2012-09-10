
;;
;; file: beyeran-gtd.el
;;
(require 'remember)

(org-remember-insinuate)

(defvar *note-file* "notes.org")
(defvar *agenda-file* "tasks/next-week.org")

(setq org-directory "~/documents/org/"

          org-agenda-file (concat org-directory *agenda-file*)
          org-default-note-file (concat org-directory *note-file*)
          
          org-agenda-include-diary t
          org-use-fast-todo-selection t
          
          org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!/!)")
                                                  (sequence "CANCELLED(c@/!)" "|" "WAITING(w@/!)"))
          
          org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                                                         ("WAITING" ("WAITING" . t))
                                                                         ("TODO" ("WAITING") ("CANCELLED"))
                                                                         ("DONE" ("WAITING") ("CANCELLED")))
          
          org-todo-keyword-faces '(("TODO"      :foreground "#ffffff" :weight bold)
                                                           ("DONE"      :foreground "cyan1"   :weight bold)
                                                           ("CANCELLED" :foreground "#454545" :weigth bold))
          
          org-capture-templates '(("t" "Todo" entry (file+headline org-agenda-file "Tasks")
                                                               "* TODO %?\n %i\n %a")
                                                          ("n" "Note" entry (file+headline org-default-note-file "Notes"
                                                                   "* %^{Title} \n%?"))))


(multiple-global-set-key (("\C-cl" org-store-link)
                                                  ("\C-ca" org-agenda)
                                                  ("\C-cb" org-iswitchb)
                                                  ("\C-cr" org-capture)))

(provide 'beyeran-gtd)
