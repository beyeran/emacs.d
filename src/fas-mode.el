(defun fas-to-regex-region (start end)
  "Takes a selected FAS (without FieldID and PT), converts it to a regex
and copies it to clipboard."
  (interactive "r")
  (kill-new
   (replace-regexp-in-string "\\(.*\\)" ".*\\1.*"
                             (replace-regexp-in-string "|<[a-z\_#0-9]+>" "\|<.*>" (buffer-substring start end)))))

(setq fas-highlights
      '(("^[^\t]+" . font-lock-function-name-face)
        ("\t\\([^\t]+\\)\t" . (1 font-lock-keyword-face))
        ("[^ \t]+\\(<\\([^u]\\|u\\([^n]\\|n[^k]\\)\\)[A-Za-z0-9#_]+>\\)" . (1 font-lock-builtin-face))
        ("\\([^ \t]+\\)<\\([^u]\\|u\\([^n]\\|n[^k]\\)\\)[a-z0-9#_]+>" . (1 font-lock-type-face))
        ("\\([^ \t]+\\)|<unk>" . font-lock-doc-face)
        ("\t" . font-lock-comment-face)))

(define-derived-mode fas-mode fundamental-mode "fas"
  "major mode for editing fas files."
  (setq font-lock-defaults '(fas-highlights)))

(add-to-list 'auto-mode-alist '("\.fieldannotatedstring" . fas-mode))

(provide 'fas-mode)
