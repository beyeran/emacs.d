
;;
;; darcsum
;;

(with-library darcsum)

;;
;; magit
;;

;; (eval-after-load 'info
;;  '(progn (info-initialize)
;;          (add-to-list 'Info-directory-list "~/.emacs.d/modules/magit/")))

;; (with-library git-commit-mode)
;; (with-library magit)

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
;; (with-library powerline
;;              (powerline-center-theme))

;;
;; perl
;;

;; (with-library sepia
;;               (setq sepia-perl5lib (list (expand-file-name "~/.emacs.d/modules/sepia/lib")))
;;               (defalias 'perl-mode 'sepia-mode))

(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)

(setq auto-insert-alist
      '(("\\.scm" .
         (insert "#!/bin/sh\n# -*- scheme -*-\nexec csi -s $0 \"$@\"\n\n"))))

(setf scheme-program-name "csi")

;;
;; haskell mode
;;
(with-library haskell-mode
              (require 'haskell-mode-autoloads)
              (add-to-list 'Info-default-directory-list "~/.emacs.d/modules/haskell-mode/")

              (add-to-alist '("\\.\\(hs\\|lhs\\)$" . org-mode))

              (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;;
;; lisp
;;
(setq inferior-lisp-program (case system-type
                                  ((windows-nt cygwin) "c:/ccl/wx86cl -K utf-8")))

;;
;; clojure
;;

(with-library clojure-mode
              (add-to-alist '("\\.\\(clj\\)$" . clojure-mode)))

;;
;; needed for cider
;;
(with-library epl)
(with-library dash)
(with-library pkg-info)

(with-library cider
              (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
              (setq nrepl-hide-special-buffers t)
              (setq cider-repl-pop-to-buffer-on-connect nil)
              (setq cider-repl-results-prefix ";; => "))

;;
;; julia
;;

(with-library julia-mode)

;;
;; APL
;;

(add-to-list 'load-path "~/.emacs.d/modules/apl")

(when (require 'gnu-apl-mode nil t)
  (dolist (hook '(gnu-apl-mode-hook gnu-apl-interactive-mode-hook))
    (add-hook hook (lambda ()
                     (eldoc-mode)
                     (setq buffer-face-mode-face 'gnu-apl-default)
                     (buffer-face-mode))))
  (set-face-attribute 'gnu-apl-default nil
                      :family "DejaVu Sans Mono")
  (add-to-list 'auto-mode-alist '("\\.apl$" . gnu-apl-mode)))

(setq gnu-apl-show-keymap-on-startup t)

(add-hook 'gnu-apl-interactive-mode-hook 
          '(lambda ()
             (setq buffer-face-mode 'gnu-apl-default)
             (buffer-face-mode)))
