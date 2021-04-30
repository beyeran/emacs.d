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
        ("[^ \t]+\\(<\\([^u]\\|u\\([^n]\\|n[^k]\\)\\)[A-Za-z0-9#_\.]+>\\)" . (1 font-lock-builtin-face))
        ("\\([^ \t]+\\)<\\([^u]\\|u\\([^n]\\|n[^k]\\)\\)[a-z0-9#_\.]+>" . (1 font-lock-type-face))
        ("\\([^ \t]+\\)|<unk>" . font-lock-doc-face)
        ("\t" . font-lock-comment-face)))

(defun apb/fas-search-mention (search-string &optional arg)
  "Search for a <unk> tagged SEARCH-STRING, remove the <unk> and place the curser there."
  (interactive "sEnter SForm: ")
  (kmacro-exec-ring-item
   `(,(vconcat '(19)
               (remove-if 'zerop (mapcar #'string-to-char (split-string (format "%s" search-string) "")))
               '(return 19 117 110 107 return backspace backspace backspace))
     0 "%d")
   arg))

(defun fas/replace-mention-by-sform (sform &optional arg)
  "Prompts for a SFORM which will be searched, the unk tag removed, and the cursor set there."
  (interactive "sEnter SForm: ")
  (isearch-forward nil 1)
  (isearch-yank-string (format "%s" sform))
  (isearch-forward nil 1)
  (isearch-yank-string "unk")
  (isearch-forward nil 1)
  (isearch-done)
  (delete-backward-char 3))

(defun fas/prepare-string-by-fromer-line (&optional arg)
  "Transforms a normal text line to a <unk> tagged fieldannotatedstring based on the previous line."
  (interactive "p")
  (kmacro-exec-ring-item (quote ([5 32 1 16 67108896 134217848 105 115 101 97 114 99 104 45 102 111 114 119 97 114 100 return 17 tab return 6 134217848 105 115 101 97 114 99 104 45 102 111 114 119 97 114 100 return 17 tab return 134217847 1 14 25 67108896 5 134217848 114 101 112 108 97 99 101 45 115 116 114 105 110 103 return 32 return 124 60 117 110 107 62 32 return backspace 1 14] 0 "%d")) arg))

(define-derived-mode fas-mode fundamental-mode "fas"
  "major mode for editing fas files."
  (setq font-lock-defaults '(fas-highlights)))

(provide 'fas-mode)
