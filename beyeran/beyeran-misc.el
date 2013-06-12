
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mac tweak                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alt-as-meta-for-mac ()
  (setq mac-command-modifier 'meta
    mac-option-modifier 'none
    default-input-method "MacOSX"))

(sys-diversification
 ()
 (alt-as-meta-for-mac))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf comment-style 'indent)

(add-hook 'emacs-lisp-mode-hook #'imenu-add-menubar-index)
(global-set-key [mouse-3] 'mouse-popup-menubar-stuff)

(display-time)
(setf display-time-day-and-date nil)
(setf display-time-24hr-format t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time stamp                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar iso-date-format "%Y-%m-%dT%H:%M:%S:z"
  "Format string for ISO dates.")

(defun iso-timestamp (&optional time)
  (format-time-sting iso-date-format
             (or time (current-time))))

(defun insert-iso-timestamp ()
  (interactive)
  (insert (iso-timestamp)))

(defun iso-timestamp-sexp (&optional time)
  (parse-time-string (iso-timestamp)))

(require 'time-stamp)

(add-hook 'before-save-hook 'time-stamp)
(setf time-stamp-active t)

(setq time-stamp-pattern "~10/^last modified: %%$")
(setq time-stamp-pattern "last modified:[ \t]+\\\\?[\"<]+ %:y-%02m-%02d %02H:%02M:%02S \\\\?[\">]")

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;;;; Custom Functions ;;;;
(defun html-umlaute ()
  "replaces iso-umlaute with html-umlaute"
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (mapconcat '(lambda (x) (car x)) *html-entities* "\\|")
              nil t)
        (replace-match (cdr (assoc (match-string 0) *html-entities*)))))))

;;;; Variables ;;;;
(setf *html-entities*
  '(("Ä" . "&Auml;")
    ("ä" . "&auml;")
    ("Ö" . "&Ouml;")
    ("ö" . "&ouml;")
    ("Ü" . "&Uuml;")
    ("ü" . "&Uuml;")
    ("ß" . "&szling;")))

(defmacro defshrink (system space)
  `(defun ,system ()
     (interactive)
     (shrink-window ,space)))

(defshrink massive-shrink-darwin 20)
(defshrink massive-shrink-linux 14)
(defshrink massive-shrink-win 25)

(global-set-key (kbd "C-x C-q")
                (sys-diversification
                 'massive-shrink-linux
                 'massive-shrink-darwin
                 'massive-shrink-win))

(setq *filestamp-user-name* "Andre Pascal Beyer"
      *filestamp-user-email* "beyeran at gmail dot com")

(defun add-comment-to-filestamp (comment)
  (concat comment " ------------------------------------------------------------ " comment "\n"
          comment "\n"
          comment " file:          " (file-name-nondirectory (buffer-file-name)) "\n"
          comment "\n"
          comment " author:        " *filestamp-user-name* "\n"
          comment " email:         < " *filestamp-user-email* " >\n"
          comment " last modified: <  >\n"
          comment "\n"
          comment " ------------------------------------------------------------ " comment "\n\n"))

(setq auto-insert-alist '((("\\.\\(tex\\|sty\\|cls\\)\\'" . "LaTeX Comment") . 
                           (insert (add-comment-to-filestamp "%%")))
                          (("\\.\\(lisp\\|lsp\\|cl\\|asd\\)\\'" . "Lisp Comment") .
                           (insert (add-comment-to-filestamp ";;")))
                          (("\\.\\(hs\\|lhs\\)\\'" . "Haskell Comment") .
                           (insert (add-comment-to-filestamp "--")))
                          (("\\.\\(sh\\|zsh\\)\\'" . "Shell Comment") .
                           (insert (add-comment-to-filestamp "##")))
                          (("\\.\\(rb\\|irb\\)\\'" . "Ruby Comment") .
                           (insert (add-comment-to-filestamp "##")))
                          (("\\.py\\'" . "Python Comment") .
                           (insert (add-comment-to-filestamp "##")))))

(add-hook 'find-file-hook 'auto-insert)

(auto-insert-mode)
(setq auto-insert-query nil)

(provide 'beyeran-misc)
