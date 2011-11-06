;;;; misc.lisp

(defun get-random-elt (some-list)
  "Returns a random element of SOME-LIST."
  (if (and some-list (listp some-list))
    (nth (random (length some-list)) some-list)
    (error "Argument to get-random-elt not a list or list is empty.")))

;; TODO: for some reason this only returns one value.
(defun get-directions (row1 col1 row2 col2)
  (list (cond ((> row2 row1) :south)
              ((< row2 row1) :north)
              ((> col2 col1) :east)
              ((< col2 col1) :west))))

(defun is-member-p (item some-list)
  (loop for i in some-list
        when (equal item i)
        return i))

;(defstruct (tconc (:constructor create-tconc))
;  "A tail concatenate list for easy append"
;  (head nil) (tail nil))
;(defun make-tconc (&optional l)
;  (let ((tc (create-tconc)))
;    (apply #'tconc tc l)
;    tc))
;(defun tconc (tconc-structure &rest items)
;  (unless (null items)
;    (if (null (tconc-head tconc-structure))
;      (setf (tconc-head tconc-structure) items)
;      (setf (cdr (tconc-tail tconc-structure)) items))
;    (setf (tconc-tail tconc-structure) (last items)))
;  (tconc-head tconc-structure))
;(defun tconc-list (tconc-structure &optional l)
;  (apply #'tconc tconc-structure l))


