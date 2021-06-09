;;; config-org.el --- Configuration for org-mode.

;;; Commentary:

;;; Code:
(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list
   '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; Hiding those emphasis markers, like /foo/ or =baz=.
(setq org-hide-emphasis-markers t)

(use-package org-fragtog
  :ensure t
  :after org
  :custom
  (org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(provide 'config-org)
;;; config-org.el ends here
