
;;
;; file: beyeran-auto-insert.el
;;

(require 'autoinsert)

(auto-insert-mode)
(setq auto-insert-query nil
      auto-insert-directory (expand-file-name "~/.emacs.d/auto-complete/"))

(add-hook 'find-file-hooks 'auto-insert)

(setq auto-insert-alist
      '(("\\.lisp$" . ["insert.lisp" auto-update-file])
        ("\\.rb$" . [ "ruby.rb" auto-update-file ])))


(defun insert-today ()
  "Insert today's date into buffer"
  (interactive)
  (insert (format-time-string "%A, %B %e %Y" (current-time))))

(defun auto-update-file ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "@@@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name))))))

(define-auto-insert "\.rb" "ruby.rb")

(provide 'beyeran-auto-insert)
