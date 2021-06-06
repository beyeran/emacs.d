(use-package org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell      . t)
                                 (java       . t)
                                 (latex      . t)
                                 (ditaa      . t)
                                 (C          . t)
                                 (emacs-lisp . t)
                                 (plantuml   . t)
                                 (dot        . t)
                                 (python     . t))))

;; Add indent mode by default
(add-hook 'org-mode-hook 'org-indent-mode)

;; Please see the `form` =latex-mode= snippet to understand more of the workflow:
(defun apb/org-mode-hook ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))

(eval-after-load 'org
  (progn
    (add-hook 'org-mode-hook #'apb/org-mode-hook)
    (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)))


(use-package org-id
  :straight nil
  :defer t
  :diminish org-indent-mode
  :hook ((org-mode . org-indent-mode)))


;; Closing
(provide 'init-org)
