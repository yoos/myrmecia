;;;; misc.lisp

(defun get-random-elt (some-list)
  "Returns a random element of SOME-LIST."
  (if (and some-list (listp some-list))
    (nth (random (length some-list)) some-list)
    (error "Argument to get-random-elt not a list or list is empty.")))

(defun get-directions (row1 col1 row2 col2)
  (list (if (> row2 row1) :south :north)   ; Row increases down
        (if (> col2 col1) :east :west)))   ; Col increases right

