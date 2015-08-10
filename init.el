;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.
;;

;;
;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
;;
(require 'cl)

(defvar *dotfiles-dir* (file-name-directory (or (buffer-file-name) load-file-name)))
(defvar *modules-dir* (concat *dotfiles-dir* "modules/"))

;;
;; loading paths
;;
(defun add-to-load-path (name)
  (add-to-list 'load-path (concat *modules-dir* name)))

(add-to-load-path (format "%s%s" *modules-dir* "org-mode"))
(add-to-load-path (format "%s%s" *modules-dir* "org-mode/lisp"))

(load (format "%s%s" *dotfiles-dir* "emacs.el"))

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" *dotfiles-dir*))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  ;; (require 'org-install)
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
;; (mapc #'org-babel-load-file (directory-files *dotfiles-dir* t "\\.org$"))
;;; init.el ends here
