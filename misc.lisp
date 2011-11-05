;;;; misc.lisp

(defun get-random-elt (some-list)
  "Returns a random element of SOME-LIST."
  (if (and some-list (listp some-list))
    (nth (random (length some-list)) some-list)
    (error "Argument to get-random-elt not a list or list is empty.")))

(defun get-directions (a-row a-col t-row t-col)
  (append (if (> t-row a-row) '(:north) '(:south))
          (if (> t-col a-col) '(:east) '(:west))))


