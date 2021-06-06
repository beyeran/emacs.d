;;   Bullets
(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; Hiding those emphasis markers, like /foo/ or =baz=.
(setq org-hide-emphasis-markers t)

(use-package org-fragtog
  :ensure t
  :after org
  ;; :custom
  ;; (org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Diverse other eyecandy. After that, you normal =org-file= should look more like an actuall word processor. Thanks internet!
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)


(provide 'config-org)
