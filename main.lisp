;;;; main.lisp

(load "ants.lisp")
(load "misc.lisp")

(defvar *dest-list-old* nil)
(defvar *dest-list-new* nil)

;;; Functions

(defun do-turn ()
  (setq *dest-list-old* *dest-list-new*)   ; Save old destination list.
  (setq *dest-list-new* nil)               ; Empty new destination list.
  (loop for ant in (my-ants *state*)
        for a-row = (elt ant 0)
        for a-col = (elt ant 1)
        do (let ((closest-distance 999999)   ; Distance to closest target.
                 (closest-target nil)        ; Coordinates of closest target.
                 (dest-dir nil)              ; Direction of destination (:north :east :south :west).
                 (dest-loc nil))             ; Destination (cell dest-dir of current location).
             (loop for target in (append (food *state*) (enemy-ants *state*))
                   for t-row = (elt target 0)
                   for t-col = (elt target 1)
                   for this-distance = (distance a-row a-col t-row t-col)   ; Distance (x^2+y^2) to target.
                   do (if (< this-distance closest-distance)
                        (progn (setf closest-distance this-distance)
                               (setf closest-target (cons t-row t-col)))))
             (labels ((dir-safe-p (dir)
                                  ;; TODO: when this is (if t), the bot runs. Right now, it crashes.
                                  (if (and (not (waterp a-row a-col dir))
                                           (not (is-member-p (new-location a-row a-col dir)
                                                             *dest-list-old*))
                                           (not (is-member-p (new-location a-row a-col dir)
                                                             *dest-list-new*)))
                                    dir)))
               (setf dest-dir (get-random-elt
                                (mapcar #'dir-safe-p
                                        (if closest-target
                                          (get-directions
                                            a-row a-col
                                            (car closest-target) (cdr closest-target))))))
               (if (not dest-dir)
                 (setf dest-dir (get-random-elt (mapcar #'dir-safe-p '(:north :east :south :west))))))
             (if dest-dir
               (progn (setf dest-loc (new-location a-row a-col dest-dir))
                      ;(setf *dest-list-old* (delete (list a-row a-col) *dest-list-old* :test #'subsetp))
                      (setf *dest-list-new* (append (list dest-loc) *dest-list-new*))
                      (issue-order a-row a-col dest-dir))
               (setf *dest-list-new* (append (list (cons a-row a-col)) *dest-list-new*))))))
;(if (and dest-dir (not (member dest-loc (my-ants *state*) :test #'subsetp)))

(defun main ()
  "Main game loop: parses the (initial) game state and calls DO-TURN and
  FINISH-TURN."
  (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
    (loop while (handler-case (peek-char nil *standard-input* nil)
                  (sb-int:simple-stream-error nil))
          for end-of-game-p = (parse-game-state)
          when end-of-game-p do (loop-finish)
          do (do-turn)
             (finish-turn))))
