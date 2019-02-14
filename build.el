(require 'org)
(require 'ob)
(require 'ob-tangle)

(require 'em-glob)

(defconst dot-files-src (if load-file-name
			    (file-name-directory load-file-name)
			  (file-name-directory (buffer-file-name))))

(defconst apb/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

(defun apb/tangle-file (file)
  "Given an `org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

(defun apb/tangle-files (path)
  "Given adirectory, PATH, of `org-mode' files, tangle source code."
  (interactive "D")
  (mapc 'apb/tangle-file (apb/get-files path)))

(defun apb/build-dot-files ()
  "Compile and deploy 'init files' in this directory."
  (interactive)
  
  ;; Initially create some of the destination directories
  (mkdir (concat apb/emacs-directory "elisp"))
  (apb/tangle-files (concat dot-files-src "*.org"))
  (message "Finished building dot-files."))

(defun apb/substring-replace (old-str new-str beg end)
  (concat (substring old-str 0 beg) new-str (substring old-str end)))

(defun apb/getvar (var-name)
  (or (getenv var-name)
      (eval (read var-name))))

(defun apb/substr-variables (str)
  (if (string-prefix-p "~/" str)
      (apb/substr-variables
       (concat (getenv "HOME") (substring str 1)))

    (let ((s (or (string-match "${\\([^ }]*\\)}" str)
		 (string-match "$\\([A-z_]*\\)" str)))
	  (e (match-end 0)))
      (if (not s)
	  str
	(apb/substr-varables
	 (apb/substring-replace str (apb/getvar (match-string 1 str)) s e))))))

(defun apb/get-files (path &optional full)
  (let ((subbed-path (apb/substr-variables path)))
    (condition-case nil
	(directory-files (file-name-directory subbed-path)
			 full
			 (eshell-glob-regexp
			  (file-name-nondirectory subbed-path)))
      (error '()))))
