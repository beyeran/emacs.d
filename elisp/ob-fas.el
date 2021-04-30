(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tagnle-lang-exts '("fas" . "fieldannotatedstring"))

(defvar org-babel-default-header-args:fas '())

(defun org-babel-execute:fas (body params)
  "Execute a block of FAS.
  This function is called by `org-babel-execute-src-block'."
  body)

(defun org-babel-prep-session:fas (session params)
  "Return an error if the :session header argument is set.
  FAS does not support sessions."
  (error "FAS sessions are nonsensical"))

(provide 'ob-fas)
