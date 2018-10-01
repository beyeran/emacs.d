(setq unit-test-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 12 b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        (modify-syntax-entry ?\" "(\"" syn-table)
        syn-table))

(setq unit-test-font-lock
      '(("(\\(:\\(intent\\|descr\\|conv\\) .*\\))" . (2 font-lock-function-name-face)) 
        ("\\(\\(@Test\\|form\\|^in\\) \\|assertPolarity(true)\\)" . font-lock-keyword-face)
        ("id=\\([A-Za-z0-9\$_]+\\)" . (1 font-lock-builtin-face))
        ("$[A-Z0-9_]+" . font-lock-warning-face)
        ("<[a-z0-9_]+>" . font-lock-warning-face)))

(define-derived-mode unit-test-mode fundamental-mode "unit-test"
  "major mode for editing unit-test files." 
  (setq font-lock-defaults '((unit-test-font-lock))) 
  (set-syntax-table unit-test-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\.test" . unit-test-mode))

(provide 'unit-test-mode)
