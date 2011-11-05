;;;; common.lisp

(in-package :ants-common)


;;; Functions

(defun current-date-time-string ()
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))


(defun defalias (function alias)
  "Defines an alias for FUNCTION, so it can be called with ALIAS as well."
  (setf (symbol-function alias) function))


(defun distance (row1 col1 row2 col2)
  "Returns the shortest distance between ROW1,COL1 and ROW2,COL2 for a grid
  that wraps around."
  (declare (inline * + - abs cols logand min rows sqrt vector)
           (optimize (speed 3))
           (type fixnum row1 col1 row2 col2))
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (the fixnum @rows) drow)))
         (mincol (min dcol (- (the fixnum @cols) dcol))))
    (declare (type fixnum minrow mincol))
    (sqrt (logand most-positive-fixnum
                  (+ (* minrow minrow) (* mincol mincol))))))

(defalias #'distance 'dist)


(defun distance2 (row1 col1 row2 col2)
  (declare (inline * + - abs cols logand min rows vector)
           (optimize (speed 3))
           (type fixnum row1 col1 row2 col2))
  (let* ((drow (abs (- row1 row2)))
         (dcol (abs (- col1 col2)))
         (minrow (min drow (- (the fixnum @rows) drow)))
         (mincol (min dcol (- (the fixnum @cols) dcol))))
    (declare (type fixnum minrow mincol))
    ;(logand most-positive-fixnum (+ (* minrow minrow) (* mincol mincol)))))
    (+ (* minrow minrow) (* mincol mincol))))  ; faster?

(defalias #'distance2 'dist2)


(defun errmsg (&rest args)
  (format @error-stream (apply #'mkstr args))
  (force-output @error-stream))


(defun host2str (host)
  (cond ((and (vectorp host) (= 4 (length host)))
         (format nil "~D.~D.~D.~D" (elt host 0) (elt host 1) (elt host 2)
                 (elt host 3)))
        (t host)))


(defun last1 (sequence)
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (- length 1)))))

(defun (setf last1) (value sequence)
  (setf (elt sequence (- (length sequence) 1)) value))


(defun logmsg (&rest args)
  (when *verbose*
    (format @log-stream (apply #'mkstr args))
    (force-output @log-stream)))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))


(defun nearby-ants (row col max-dist2 &optional (exclude -1))
  (declare (inline + - /= = antp distance floor not sqrt pid tile-if-reachable)
           (optimize (speed 3))
           (type fixnum row col max-dist2 exclude))
  (loop with dist = (floor (sqrt max-dist2))
        for roff from (- row dist) to (+ row dist)
        append (loop for coff from (- col dist) to (+ col dist)
                     for tile = (tile-if-reachable max-dist2 row col roff coff)
                     when (and (antp tile) (/= exclude (the fixnum (pid tile)))
                               (not (and (= roff row) (= coff col))))
                       collect tile)))


(defun new-location (row col direction)
  "Returns #(NEW-ROW NEW-COL) for ROW,COL and DIRECTION for a grid that
  wraps around."
  (wrapped-row-col (cond ((equal direction :north) (- row 1))
                         ((equal direction :south) (+ row 1))
                         (t row))
                   (cond ((equal direction :west) (- col 1))
                         ((equal direction :east) (+ col 1))
                         (t col))))


(defun par-value (string)
  "Helper function for parsing game state input from the server."
  (parse-integer (subseq string (position #\space string) (length string))))


(defun print-game-map (game-map &optional (stream *debug-io*) (no-frills nil))
  (unless no-frills
    (loop with dim = (array-dimensions game-map)
          for col from 0 below (second dim)
          initially (princ #\space stream)
                    (princ #\space stream)
          do (princ (mod col 10) stream)
          finally (terpri stream)))
  (loop with dim = (array-dimensions game-map)
        for row from 0 below (first dim)
        do (if no-frills
               (princ "m " stream)
               (progn (princ (mod row 10) stream)
                      (princ #\space stream)))
           (loop for col from 0 below (second dim)
                 for tile = (aref game-map row col)
                 for type = (type-of tile)
                 do (case type
                      (land  (princ #\. stream))
                      (water (princ #\% stream))
                      (food  (princ #\* stream))
                      (ant (if (dead tile)
                               (princ (code-char (+ (pid tile) 65)) stream)
                               (princ (code-char (+ (pid tile) 97)) stream)))
                      (t (princ #\? stream))))
           (terpri stream)))


;; grabbed from Clon
(defun quit (&optional (status 0))
  "Quit the current application with STATUS."
  #+abcl  (extensions:exit :status status)
  #+ccl   (ccl:quit status)
  #+clisp (ext:exit status)
  #+cmu   (unix:unix-exit status)
  #+ecl   (ext:quit status)
  #+sbcl  (sb-ext:quit :unix-status status)
  #-(and abcl ccl clisp cmu ecl sbcl) (cl-user::quit))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun slimesg (&rest args)
  #+swank (let ((swank:*globally-redirect-io* t))
            (when *verbose*
              (format t (apply #'mkstr args))
              (force-output t)))
  #-swank (apply #'logmsg args))


(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))


(defun tile-at (row col)
  (declare (inline aref game-map)
           (optimize (speed 3)))
  (aref @game-map row col))

(defun (setf tile-at) (value row col)
  (setf (aref @game-map row col) value))


(defun tile-if-reachable (radius2 src-row src-col dst-row dst-col)
  (declare (inline <= dist2 tile-at wrapped-row wrapped-col)
           (optimize (speed 3))
           (type fixnum radius2 src-row src-col dst-row dst-col))
  (let ((wrow (wrapped-row dst-row))
        (wcol (wrapped-col dst-col)))
    (when (<= (the fixnum (dist2 src-row src-col wrow wcol)) radius2)
      (tile-at wrow wcol))))


(let ((time-units (/ 1.0 internal-time-units-per-second)))
  ;; TODO correctly name function: doesn't return wall time
  (defun wall-time (&key (offset 0))
    "Returns the time in seconds (as a FLOAT) since SBCL was started."
    (+ (* (get-internal-real-time) time-units)
       offset)))


(defun wrapped-row (row)
  (declare (inline + - < >= rows)
           (optimize (speed 3))
           (type fixnum row))
  (let ((rs (the fixnum @rows)))
    (cond ((< row 0) (+ rs row))  ; adding negative number
          ((>= row rs) (- row rs))
          (t row))))


(defun wrapped-col (col)
  (declare (inline + - < >= cols)
           (optimize (speed 3))
           (type fixnum col))
  (let ((cs (the fixnum @cols)))
    (cond ((< col 0) (+ cs col))  ; adding negative number
          ((>= col cs) (- col cs))
          (t col))))


;; TODO use wrapped-row and wrapped-col
(defun wrapped-row-col (row col)
  (declare (inline + - < >= cols rows vector)
           (optimize (speed 3))
           (type fixnum row col))
  (let ((rs (the fixnum @rows))
        (cs (the fixnum @cols)))
    (vector (cond ((< row 0) (+ rs row))  ; adding negative number
                  ((>= row rs) (- row rs))
                  (t row))
            (cond ((< col 0) (+ cs col))  ; adding negative number
                  ((>= col cs) (- col cs))
                  (t col)))))
