
;;
;; file: beyeran-org.el
;;

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-log-done t)
(setq org-support-shift-select t)
(setq org-src-fontify-natively t)

(defun org-cycle-global ()
  (interactive)
  (org-cycle t))

(defun org-cycle-local ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cycle)))

(setq org-export-with-section-numbers nil)

(provide 'beyeran-org)
