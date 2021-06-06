;;; init-elisp.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Personal configuration for emacs lisp mode. As always still under
;; development.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2021-06-06
;;  * Renaming
;; 2021-04-30
;;  * Refactor to this form
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'cl-lib)

(use-package lisp-mode
  :init
  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)                  ; Shrink this
      ("."       . ?•)))                ; Enlarge this

  :bind (("C-c e i" . ielm))

  :config
  (add-hook 'emacs-lisp-mode-hook 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

  ;; Bind some prefixes to a couple of mode maps:
  (bind-keys :map emacs-lisp-mode-map
             :prefix-map lisp-find-map
             :prefix "C-h e"
             ("e" . view-echo-area-messages)
             ("f" . find-function)
             ("k" . find-function-on-key)
             ("l" . find-library)
             ("v" . find-variable)
             ("V" . apropos-value))

  (dolist (m (list emacs-lisp-mode-map lisp-interaction-mode-map))
    (bind-keys :map m
               :prefix-map lisp-evaluation-map
               :prefix "C-c e"
               ("b" . eval-buffer)
               ("r" . eval-region)
               ("c" . eval-and-comment-output) ;; Defined below
               ("o" . eval-and-comment-output)
               ("d" . toggle-debug-on-error)
               ("f" . emacs-lisp-byte-compile-and-load))))

;;
;; IDO everywhere
;;
(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

        Set it to nil using let in around-advice for functions where the
        original completing-read is required.  For example, if a function
        foo absolutely must use the original completing-read, define some
        advice like this:

        (defadvice foo (around original-completing-read-only activate)
          (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
    (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

;;
;; Nicer patter tester
;;
(use-package paren
  :init
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#afa")
  (set-face-attribute  'show-paren-match nil :weight 'black)
  (set-face-background 'show-paren-mismatch (face-background 'default))
  (set-face-foreground 'show-paren-mismatch "#c66")
  (set-face-attribute  'show-paren-mismatch nil :weight 'black))

(use-package paren-face
  :ensure t
  :init
  (global-paren-face-mode))

(add-hook 'after-save-hook 'check-parens nil t)

;;
;; ielm
;;
(use-package ielm
  :init
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

;; Instead of displaying the results in a separate buffer (like the
;; above code does), The https://github.com/xiongtx/eros displays the results temporarily
;; /in the buffer/ in an overlay.  No need to do anything special:
(use-package eros
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (eros-mode 1))))


;; While writing and documenting Emacs Lisp code, it would be helpful
;; to insert the results of evaluation of an s-expression directly
;; into the code as a comment:

(defun current-sexp ()
  "Returns the _current expression_ based on the position of the
       point within or on the edges of an s-expression."
  (cond
   ((looking-at "(") (sexp-at-point))
   ((looking-back ")" 1) (elisp--preceding-sexp))
   (t (save-excursion
        (search-backward "(")
        (sexp-at-point)))))

(defun eval-current-sexp ()
  "Evaluates the expression at point. Unlike `eval-last-sexp',
     the point doesn't need to be at the end of the expression, but
     can be at the beginning (on the parenthesis) or even somewher
     inside."
  (interactive)
  (eval-expression (current-sexp)))

(defun eval-and-comment-output ()
  "Add the output of the `current-sexp' as a comment at the end
     of the line. Calling this multiple times replaces the comment
     with the new evaluation value."
  (interactive)
  (let* ((marker " ; -> ")
         (expression (current-sexp))
         (results (eval expression)))
    (save-excursion
      (beginning-of-line)
      (if (search-forward marker (line-end-position) t)
          (delete-region (point) (line-end-position))
        (end-of-line)
        (insert marker))
      (condition-case nil
          (princ (pp-to-string results) (current-buffer))
        (error (message "Invalid expression"))))))

;;
;; Paredit
;;
(use-package paredit
  :ensure t
  :diminish "﹙﹚"
  :init
  (dolist (m (list 'emacs-lisp-mode-hook
                   'lisp-interaction-mode-hook
                   'eval-expression-minibuffer-setup-hook
                   'ielm-mode-hook))
    (add-hook m 'enable-paredit-mode)))

;;
;; Code Navigation
;;
(use-package lispy
  :ensure t
  :defer t
  :bind (:map lispy-mode-map
              ("C-1" . nil)
              ("C-2" . nil)
              ("C-3" . nil)
              ("C-4" . nil))
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1)))))


(provide 'init-elisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elisp.el ends here
