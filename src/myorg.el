
;; (require 'ox-beamer)

;;
;; ess
;;
(add-to-list 'load-path "~/.emacs.d/modules/ess-site/lisp")
(require 'ess-site)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (latex . t)
   (haskell . t)
   (emacs-lisp . t)
   (ruby . t)
   (sh . t)
   (lisp . t)
   (octave . t)
   (octave . t)))

(add-hook 'inferior-octave-mode-hook
          '(lambda ()
             (setq inferior-octave-program
                   "C:\\cygwin\\usr\\bin\\octave.exe")))

;; (setq org-babel-R-command "c:/Program\ Files/R/R-3.0.2/bin/x64/Rterm.exe --slave --no-save")

;;
;; latex
;;
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(server-start)

(require 'org-latex)

(defun org-export-latex-no-toc (depth)  
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
            depth)))
(setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

(add-to-list 'org-export-latex-classes
             '("memarticle"
               "\\documentclass[11pt,oneside,article]{memoir}\n % \\input{vc} % vc package"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
             '("mempaper"
               "\\documentclass[11pt,oneside,article]{memoir}\n % \\input{vc} % vc package"
               ("\\section{%s}" . "\\section{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph{%s}")))
  
(add-to-list 'org-export-latex-classes
             '("membook"
               "\\documentclass[11pt,oneside]{memoir}\n % \\input{vc} % vc package"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

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

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.22 or higher.  
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
                     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: pdflatex 
  (setq texcmd "latexmk -pdflatex -synctex=1 --shell-escape -pdf %f")        
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex -synctex=1 --shell-escape -pdf %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -xelatex -synctex=1 --shell-escape -pdf %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)
  
;; Default packages included in /every/ tex file, latex, pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" )))
  
;; Custom packages
(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include. See https://github.com/kjhealy/latex-custom-kjh for the support files included here."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-export-latex-default-packages-alist
        '(("AUTO" "inputenc" t)
          ("minted,minion" "org-preamble-pdflatex" t)))        
  ;; Packages to include when xelatex is used
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-default-packages-alist
            '(("minted" "org-preamble-xelatex" t) ))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-XeTeX-command "latexmk -xelatex -synctex=1")
 '(TeX-engine (quote xetex))
 ;; '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))
 ;; '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))
 '(show-paren-mode t)
 '(blink-cursor-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reftex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'reftex)

(setq reftex-default-bibliography
      '("G:\\Dropbox\\literature\\master.bib"))
;; I only use this, because the former does not seem to work somehow
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")) 

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         (reftex-parse-all)
         ;; add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bib:%l][%l-bib]]")
            (?p . "** [[papers:%l][%l]]: %t \n"))))))

(define-key org-mode-map (kbd "C-c )") 'reftex-citation)
(define-key org-mode-map (kbd "C-c (") 'org-reftex-citation)
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-link-abbrev-alist
      '(("bib" . "G:/Dropbox/literature/master.bib::%s")
        ("papers" . "G:/Dropbox/literature/papers/%s.pdf")))
;; adding auctex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-inoto-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Look
(add-hook 'latex-mode-hook 
          (lambda ()
            (set-face-attribute 'font-latex-sectioning-5-face nil :inherit nil :foreground "#b58900")
            (set-face-attribute 'font-latex-sectioning-0-face nil :height 3)
            (set-face-attribute 'font-latex-sectioning-1-face nil :height 2)
            (set-face-attribute 'font-latex-sectioning-2-face nil :height 1.5)
            (set-face-attribute 'font-latex-sectioning-3-face nil :height 1.2)
            (set-face-attribute 'font-latex-sectioning-4-face nil :height 1.0)))

 (add-hook 'org-mode-hook 
           (lambda ()
             (set-face-attribute 'org-level-1 nil :height 1.5)
             (set-face-attribute 'org-level-2 nil :height 1.2)
             (set-face-attribute 'org-level-3 nil :height 1.1)
             (set-face-attribute 'org-level-4 nil :height 1.1)
             (set-face-attribute 'org-level-5 nil :height 1.1)))

;; Set to the location of your Org files on your local system
(setq org-directory "G:/Dropbox/org/org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "G:/Dropbox/org/mobile")
;; Set to the files (or directory of files) you want sync'd
(setq org-agenda-files '("G:/Dropbox/org/org"))
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "G:/Dropbox/org/org/from-mobile.org")
