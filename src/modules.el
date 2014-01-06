
;;
;; darcsum
;;

(with-library darcsum)

;;
;; magit
;;

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/modules/magit/")))

(with-library git-commit-mode)
(with-library magit)

;;
;; paredit
;;
(defun add-paredit (mode)
  "Wrapps the function used for adding paredit to mode hooks"
  (add-hook mode #'enable-paredit-mode))

(with-module enable-paredit-mode "paredit"
             (add-paredit 'emacs-lisp-mode)
             (add-paredit 'eval-expression-minibuffer-setup-hook)
             (add-paredit 'ielm-mode-hook)
             (add-paredit 'lisp-mode-hook)
             (add-paredit 'lisp-interaction-mode-hook)
             (add-paredit 'scheme-mode-hook)
             (add-paredit 'python-mode-hook))

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;
;; powerline
;;
(with-library powerline
              (powerline-center-theme))

;;
;; perl
;;

(with-library sepia
              (setq sepia-perl5lib (list (expand-file-name "~/.emacs.d/modules/sepia/lib")))
              (defalias 'perl-mode 'sepia-mode))

(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)

(setq auto-insert-alist
      '(("\\.scm" .
         (insert "#!/bin/sh\n# -*- scheme -*-\nexec csi -s $0 \"$@\"\n\n"))))

(setf scheme-program-name "csi")
