;; General
(defun apb/org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'apb/org-latex-yas)


;; Modes for writing
(use-package writegood-mode
  :ensure t
  :after org
  :config
  (custom-theme-set-faces
   'user
   `(writegood-weasels-face ((t (:foreground "#ff0000"))))
   `(writegood-passive-voice-face ((t (:foreground "#ff0000"))))
   `(writegood-duplicates-face ((t (:foreground "#ff0000"))))))

(use-package visual-fill-column
  :ensure t
  :after org
  :hook ((visual-line-mode . visual-fill-column-mode)
         (org-mode . visual-line-mode))
  :diminish auto-fill-mode
  :straight (:host github :repo "joostkremers/visual-fill-column"))

(setq visual-fill-column-width 120)

(use-package writeroom-mode
  :ensure t
  :after org
  :straight (:host github :repo "joostkremers/writeroom-mode"))




;; Latex
(setq org-latex-pdf-process '("xelatex -shell-escape %f"))

(when (eq system-type 'darwin)
  (setq org-latex-pdf-process '("/Library/TeX/texbin/xelatex -quiet -shell-escape %f")))

(setq org-latex-listings 'minted)

;; Latex Export Template
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("article"
                 "% -------------------
% Packages
% -------------------
\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8x]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{mathptmx} % Use Times Font


\\usepackage[pdftex]{graphicx} % Required for including pictures
\\usepackage[german]{babel}
\\usepackage[pdftex,linkcolor=black,pdfborder={0 0 0}]{hyperref} % Format links for pdf
\\usepackage{calc} % To reset the counter in the document after title page
\\usepackage{enumitem} % Includes lists

\\frenchspacing % No double spacing between sentences
\\linespread{1.2} % Set linespace
\\usepackage[a4paper, lmargin=0.1666\\paperwidth, rmargin=0.1666\\paperwidth, tmargin=0.1111\\paperheight, bmargin=0.1111\\paperheight]{geometry} %margins

\\usepackage[all]{nowidow} % Tries to remove widows
\\usepackage[protrusion=true,expansion=true]{microtype} % Improves typography, load after fontpackage is selected
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("exercise"
                 "\\documentclass{tufte-handout}

\\usepackage[ngerman, english]{babel}

\\setmainfont{Adobe Garamond Pro}
\\setsansfont{Adobe Caslon Pro}
\\setmonofont{FiraCode Nerd Font Mono}

\\usepackage{geometry}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\PassOptionsToPackage{normalem}{ulem}
\\usepackage{ulem}
\\usepackage{amsthm}
\\usepackage{polynom}
\\usepackage{mathtools}

\\pagestyle{empty}
\\setlength\\parskip{0.5em}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;; General Babel And Loading
(use-package ox-pandoc
  :ensure t
  :config
  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  ;; special extensions for markdown_github output
  (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html)))

(provide 'init-writing)
