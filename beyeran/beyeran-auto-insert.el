
(require 'autoinsert)

(auto-insert-mode)
(setq auto-insert-query nil)
(setq auto-insert-directory (expand-file-name "~/.emacs.d/auto-complete/"))

(add-hook 'find-file-hooks 'auto-insert)

(setq auto-insert-alist
      '(("\\.lisp$" . ["insert.lisp" auto-update-lisp-file])))


(defun insert-today ()
  "Insert today's date into buffer"
  (interactive)
  (insert (format-time-string "%A, %B %e %Y" (current-time))))

(defun auto-update-lisp-file ()
  (save-excursion
        ;; Replace @@@ with file name
        (while (search-forward "@@@" nil t)
          (save-restriction
            (narrow-to-region (match-beginning 0) (match-end 0))
            (replace-match (file-name-nondirectory buffer-file-name))))))


(provide 'beyeran-auto-insert)
