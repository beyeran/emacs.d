
;;
;; file: beyeran-org.el
;;

(require 'org)
(require 'org-id)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-log-done t
      org-support-shift-select t
          org-src-fontify-natively t
          org-export-with-section-numbers nil)

;; overwriting some org functions
(defun org-cycle-global ()
  (interactive)
  (org-cycle t))

(defun org-cycle-local ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cycle)))

(provide 'beyeran-org)
