
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
(defvar *src-dir* (concat *dotfiles-dir* "src/"))

;; (load (format "%s%s" *dotfiles-dir* "emacs.el"))

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

;;
;; helper
;;
(defmacro on-win (&rest body)
  `(when (equal system-type 'windows-nt)
     (progn 
       ,@body)))
  
(defmacro on-linux (&rest body)
  `(when (equal system-type 'gnu/linux)
     (progn 
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;;
;;;; obtain el-git
;;;;
(mapcar #'(lambda (n) (add-to-list 'load-path n))
        '("~/.emacs.d/el-get/el-get"))
  
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
       (lambda (s)
         (goto-char (point-max))
         (eval-print-last-sexp)))))
  
;;;;
;;;; initialization
;;;;
(require 'el-get)
  
;; recipe (copied)
(setq el-get-sources
      '((:name el-get :branch "master")
        (:name magit
               :before (global-set-key (kbd "C-x C-z") 'magit-status))
        (:name goto-last-change
               :before (global-set-key (kbd "C-x C-/") 'goto-last-change))))
  
(setq beyeran-packages
      (append
       '(paredit cygwin-mount color-theme-darktooth
                 git-gutter flyspell flymake helm elixir
                 rainbow-delimiters rainbow-identifiers
                 highlight-indentation org-jekyll lua-mode
                 powerline)
  
       (mapcar 'el-get-as-symbol
               (mapcar 'el-get-source-name el-get-sources))))
  
;; needed
(add-to-list 'load-path "~/.emacs.d/el-get/ess/lisp")
  
(el-get 'sync beyeran-packages)
  
;; yeah, something weird happend with org-mode
(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remember tangle files before!
(mapc #'load (directory-files "~/.emacs.d/src/" t "\\.el$"))
