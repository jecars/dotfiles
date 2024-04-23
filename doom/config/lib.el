
;; -------------------- Useful functions --------------------

(defun jecs/toggle-quote-lines (beg end)
  "Toggle wrapping all items in region with double quotes."
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "no region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"" item)))
                              (split-string (buffer-substring beg end) "\n"))
                      "\n")))
    (delete-region beg end)
    (insert replacement)))

(defun jecs/stare ()
  (interactive)
  (let ((image-list '("~/Pictures/o_o/Gregory.png"
                      "~/Pictures/o_o/hamsterStare.png"
                      "~/Pictures/o_o/monkaStare.png"
                      "~/Pictures/o_o/Stare.png"
                      "~/Pictures/o_o/crunchy-cat-luna.png"
                      "~/Pictures/o_o/mikeStare.png")))
    (ffap (nth (random (length image-list)) image-list)))
  (image-transform-fit-to-window))

(defun jecs/listening (arg)
  (interactive "P")
  (ffap
   (if arg
       "~/Pictures/library/not-listening.gif"
     "~/Pictures/library/listening.gif"))
  (image-transform-fit-to-window)
  (image-toggle-animation)
  (message "%s" arg))
