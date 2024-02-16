
;; -------------------- Useful functions --------------------

(defun jecs/toggle-quote-lines (beg end)
  "Toggle wrapping all items in region with double quotes."
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "no region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcnar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"" item)))
                              (split-string (buffer-substring beg end) "\n"))
                      "\n")))
    (delete-region beg end)
    (insert replacement)))
