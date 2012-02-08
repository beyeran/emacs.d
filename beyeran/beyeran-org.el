
;;;;;;;; org-mode ;;;;;;;;
(require 'org)
(require 'tempo)
;(require 'notify)
;(require 'org-remember)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-agenda-files (list "~/Documents/org/uni.org"
                             "~/Documents/org/freetime.org"
                             "~/Documents/org/home.org"
                             "~/Documents/org/dates.org"
                             "~/Documents/org/music.org"))

(setq org-directory "~/Documents/org/")
(setq org-default-notes-file "~/Documents/org/dairy")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c r") 'org-remember)

(setq org-log-done t)
(setq org-support-shift-select t)
(setq org-agenda-include-diary t)
(setq org-src-fontify-natively t)
(setq tempo-interactive t)


(setq org-todo-keyword-faces '(("NEXT" . (:foreground "yellow" :background "red" :bold t :weight bold))
                               ("TODO" . (:foreground "yellow" :background "forestgreen" :bold t :weight bold))
                               ("WAITING" . (:foreground "magenta" :background "red" :bold t :weight bold))
                               ("TESTING" . (:foreground "cyan" :bold t :weight bold))
                               ("RELEASED" . (:foreground "greenyellow" :bold t :weight bold))
                               ("PLANNED" . (:foreground "gray70" :bold t :weight bold))
                               ("DONE" . (:foreground "goldenrod" :bold t :weight bold))))

(setq org-startup-folded nil)

(defun org-cycle-global ()
  (interactive)
  (org-cycle t))

(defun org-cycle-local ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cycle)))

(setq org-export-with-section-numbers nil)

;;;; Forntifying todo items outside of org-mode ;;;;
;; thanks to David O'Toole
(defface todo-comment-face '((t (:background "red" :foreground "yellow" :weight bold :bold t))) "Face for TODO in code buffers.")

(defvar todo-comment-face 'todo-comment-face)
(defun fontify-todo ()
  (font-lock-add-keywords nil '(("\\<\\(TODO\\)\\>"
                                 (1 todo-comment-face t)))))

(add-hook 'emacs-lisp-mode-hook #'fontify-todo)
(add-hook 'lisp-mode-hook #'fontify-todo)

(defface headline-face '((t (:foreground "white" :underline "white" :background "navyblue"))) "Face for headlines.")

(defun fontify-headline ()
  (font-lock-add-keywords nil '(("^;;;;* \\(.*\\)\\>"
                                 (1 headline-face t)))))

(add-hook 'emacs-lisp-mode-hook #'fontify-headline)
(add-hook 'lisp-mode-hook #'fontify-headline)

(provide 'beyeran-org)
