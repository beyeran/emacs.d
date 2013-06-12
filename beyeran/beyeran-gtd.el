
;;
;; ----[ GTD ]----
;;
;; file: beyeran-gtd.el
;;
;; This is a day planer adaption seen on:
;; http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/
;;

(define-prefix-command 'org-todo-state-map)
     
(define-key org-mode-map "\C-cx" 'org-todo-state-map)
(define-key org-todo-state-map "x"
  #'(lambda nil (interactive) (org-todo "CANCELLED")))
(define-key org-todo-state-map "d"
  #'(lambda nil (interactive) (org-todo "DONE")))
(define-key org-todo-state-map "f"
  #'(lambda nil (interactive) (org-todo "DEFERRED")))
(define-key org-todo-state-map "w"
  #'(lambda nil (interactive) (org-todo "WAITING")))

(require 'remember)

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map [(control meta ?r)] 'remember)

(sys-diversification
 (custom-set-variables
  '(org-agenda-files (quote ("~/projects/gtd/todo.org")))
  '(org-default-note-file "~/projects/gtd/notes.org" ))
 (custom-set-variables
  '(org-agenda-files (quote ("~/Projects/gtd/todo.org")))
  '(org-default-note-file "~/Projects/gtd/notes.org" )))

(sys-diversification
 (setq org-remember-templates
       '((116 "* TODO %?\n   %u" "~/projects/gtd/todo.org" "Tasks")
         (110 "* %u %?" "~/projects/gtd/notes.org" "Notes")))
 (setq org-remember-templates
       '((116 "* TODO %?\n   %u" "~/Projects/gtd/todo.org" "Tasks")
         (110 "* %u %?" "~/Projects/gtd/notes.org" "Notes"))))

(custom-set-variables
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))

 '(org-agenda-custom-commands
   '(("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda "" ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
        (lambda ()
          (org-agenda-skip-entry-if 'noteregexp "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo ""
      ((org-agenda-skip-function
        (lambda ()
          (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: ")))))

 '(org-remember-store-without-prompt t)
 '(remember-annotation-functions (quote (org-remember-annotation)))

 '(remember-handler-functions (quote (org-remember-handler))))

(provide 'beyeran-gtd)
