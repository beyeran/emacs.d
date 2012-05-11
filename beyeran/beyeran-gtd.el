
(require 'remember)

;;(define-key global-map "\C-cl" 'org-store-link)
(sys-diversification 
 (setq *gtd-base-path* "~/Projects/gtd/")
 (setq *gtd-base-path* "~/projects/gtd/"))

(setq *uni-path* (concat *gtd-base-path* "uni.org"))
(setq *private-path* (concat *gtd-base-path* "private.org"))
(setq *misc-path* (concat *gtd-base-path* "misc.org"))

(setq org-agenda-files (list *uni-path*
                             *private-path*
                             *misc-path*))

;;(defmacro todo-key (key action)
;;  `(define-key org-todo-state-map ,key
;;
;;     #'(lambda () (interactive) (org-todo ,action))))

;;(todo-key "x" "CANCELLED")
;;(todo-key "d" "DONE")
;;(todo-key "w" "WAITING")

(define-key org-agenda-mode-map "\C-n" 'next-line)
(define-key org-agenda-keymap "\C-n" 'next-line)
(define-key org-agenda-mode-map "\C-p" 'previous-line)
(define-key org-agenda-keymap "\C-p" 'previous-line)

(setq org-agenda-custom-commands
      '(("P" "Project List"
         ((tags "PROJECT")))
        ("U" "University"
         ((agenda) 
          (tags "UNI")))
        ("P" "Private"
         ((agenda)
          (tags "PRIVATE")))
        ("H" "Hobby"
         ((agenda)
          (tags "HOBBY")))))

(setq org-remember-templates
      '(("Uni" ?u "- [ ] %T %^{Description}\n %x"
         *uni-path* "Uni Task")
        ("Private" ?p "- [ ] %T %^{Description}\n %x"
         *uni-path* "Private Task")))

(provide 'beyeran-gtd)
