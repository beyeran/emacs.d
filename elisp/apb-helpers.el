;; Special Sorting

;;   I often need to sort things inline, e.g. for signatures or AVMs in
;;   JSGFs. I wrote a function which sorts everything marked wheras a
;;   special sign is used as a field separator ('__' for signatures and
;;   ',' for AVMs, etc.). I found a preliminary version here
;;   [[https://www.emacswiki.org/emacs/SortWords][https://www.emacswiki.org/emacs/SortWords]] which I will use until I
;;   find time do adjust it further more to my needs.


(defun apb/sort-words (reverse beg end)
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;; Closing


(provide 'apb-helpers)
