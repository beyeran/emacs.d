
;; (require 'ox-beamer)

;;
;; ess
;;
;; (add-to-list 'load-path "~/.emacs.d/modules/ess-site/lisp")
;; (require 'ess-site)

;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((R . t)
;;     (python . t)
;;     (octave . t)))

;; (setq org-babel-R-command "c:/Program\ Files/R/R-3.0.2/bin/x64/Rterm.exe --slave --no-save")

;;
;; latex
;;
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(server-start)

(add-hook 'LaTeX-mode-hook (lambda ()
  (push 
    '("Latexmk" "latexmk -xelatex %s" TeX-run-TeX nil t
      :help "Run Latexmk on file")
    '("%(-PDF)"
      (lambda ()
        (if (and (not TeX-Omega-mode)
                 (or TeX-PDF-mode TeX-DVI-via-PDFTeX))
            "-xelatex" "")))
    TeX-command-list)))


(custom-set-variables
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("Acrobat Reader" "c:/Program Files (x86)/Adobe/Reader 11.0/Reader/AcroRd32.exe")))))
(custom-set-faces)
