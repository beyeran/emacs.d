
;;;; haml ;;;;
(defun add-haml-hooks ()
  (add-hook 'haml-mode-hook
  '(lambda ()
         (setq indent-tabs-mode nil)
         (define-key haml-mode-map "\C-m" 'newline-and-indent))))

(when (require 'haml-mode)
  (add-haml-hooks))

(provide 'beyeran-haml)
