;;;; main.lisp

(load "ants.lisp")
(load "misc.lisp")

;;; Functions

;; This is the actual 'AI'.  Very simple currently: loops through each of your
;; ants and issues an order to go either north, east, south or west if the tile
;; in the direction is not a water tile.
(defun do-turn ()
  (let ((destinations))
    (loop for ant in (reverse (my-ants *state*))   ;; You don't have to reverse the MY-ANTS list, but we did this to get the same output as most other starter bots when called with example input from the wiki.
          for a-row = (elt ant 0)
          for a-col = (elt ant 1)
          do (let ((closest-distance 999999)
                   (closest-target)
                   (directions))
               (loop for target in (append (food *state*) (enemy-ants *state*))
                     for t-row = (elt target 0)
                     for t-col = (elt target 1)
                     ;for this-distance = (distance a-row a-col t-row t-col)
                     do (let ((this-distance (distance a-row a-col t-row t-col)))
                          (if (< this-distance closest-distance)
                            (setf closest-distance this-distance)
                            (setf closest-target (list t-row t-col)))))
               ;(append destinations '(if (closest-target) closest-target (a-row a-col)))
               (issue-order a-row a-col (get-random-elt (get-directions a-row a-col (car closest-target) (cadr closest-target))))))))
               ;(issue-order a-row a-col (get-random-elt (mapcar (lambda (dir)
               ;                                                   (if (not (waterp a-row a-col dir)) dir))
               ;                                                 '(:north :east :south :west))))))))

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
