(defmacro with-module (symbol name-string &rest body)
  `(condition-case nil
       (progn
         (add-to-load-path  ,(format "%s%s" *modules-dir* name-string))
         (autoload ',symbol ,name-string ,name-string t)
         ,@body)
     
     (error (message (format " => problem loading %s" ',symbol))
            nil)))

(defmacro with-library (symbol &rest body)
  `(condition-case nil
       (progn
         (add-to-load-path ,(format "%s%s" *modules-dir* symbol))
         (require ',symbol)
         ,@body)))
