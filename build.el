(require 'org)
(require 'ob)
(require 'ob-tangle)

(defconst dot-files-src (if load-file-name
			    (file-name-directory load-file-name)
			  (file-name-directory (buffer-file-name))))

(defconst apb/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun apb/tangle-file (file)
  "Given an `org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-bable-tangle)
  (kill-buffer))

(defun apb/tangle-files (path)
  "Given adirectory, PATH, of `org-mode' files, tangle source code."
  (interactive "D")
  (mapc 'apb/tangle-file (directory-files path)))

(defun apb/build-dot-files ()
  "Compile and deploy 'init files' in this directory."
  (interactive)
  
  ;; Initially create some of the destination directories
  (mkdir (concat apb/emacs-directory "src"))
  (apb/tangle-files (concat dot-files-src "*.org"))
  (message "Finished building dot-files."))

