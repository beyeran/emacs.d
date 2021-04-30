(use-package python
  :mode ("\\.py\\'" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)
  (setq python-shell-interpreter "python3")

  :config
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'inferior-python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'color-identifiers-mode))

;; (use-package jedi
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-jedi)
;;   :config
;;   (use-package company-jedi
;;     :ensure t
;;     :init
;;     (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
;;     (setq company-jedy-python-bin "python")))

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; (use-package elpy
;;   :ensure t
;;   :commands elpy-enable
;;   :init (with-eval-after-load 'python (elpy-enable))

;;   :config
;;   (electric-indent-local-mode -1)
;;   (delete 'elpy-module-highlight-indentation elpy-modules)
;;   (delete 'elpy-module-flymake elpy-modules)

;;   (defun apb/elpy-goto-definition ()
;;     (interactive)
;;     (condition-case err
;;         (elpy-goto-defintion)
;;       ('error (xref-find-definitions (symbol-name (symbol-at-point))))))

;;   :bind (:map elpy-mode-map ([remap elpy-goto-definition] .
;;                              apb/elpy-goto-defintion)))

(provide 'apb-python)
