;;;; replay.lisp

(in-package :play-game)


;;; Functions

;; See: https://github.com/aichallenge/aichallenge/wiki/Ants-replay-format
;; Perhaps I should have used a JSON lib :-|
(defun save-replay (&optional (round 0))
  (with-open-file (f (mkstr @replay-dir round ".replay")
                   :direction :output :if-exists :supersede)
    (format f (mkstr "{~%"
                     "    \"challenge\": \"ants\",~%"
                     "    \"game_id\": 0,~%"
                     "    \"location\": \"localhost\",~%"
                     "    \"player_info\": ["))
    (loop for i from 0 below @n-players
          do (princ "{}" f)
             (when (< i (- @n-players 1))
               (princ ", " f)))
    (format f (mkstr "],~%"
                     "    \"rank\": ["))
    (loop with ranking = (loop for bot across @bots
                               collect (last1 (scores bot)) into scores
                               finally (return (sort (remove-duplicates scores)
                                                     #'>)))
          for bot across @bots
          for i from 1
          do (princ (position (last1 (scores bot)) ranking) f)
             (when (< i @n-players)
               (princ ", " f)))
    (format f (mkstr "],~%"
                     "    \"replayformat\": \"json\",~%"
                     "    \"replaydata\": {~%"
                     "        \"revision\": 2,~%"
                     "        \"players\": " @n-players ",~%"
                     "        \"loadtime\": " @load-time ",~%"
                     "        \"turntime\": " @turn-time ",~%"
                     "        \"turns\": " @turns ",~%"
                     "        \"viewradius2\": " @view-radius2 ",~%"
                   "        \"attackradius2\": " @attack-radius2 ",~%"
                     "        \"spawnradius2\": " @spawn-radius2 ",~%"
                     "        \"map\": {~%"
                     "             \"rows\": " @rows ",~%"
                     "             \"cols\": " @cols ",~%"
                     "             \"data\": [~%"))
    (loop for row from 0 below @rows
          do (format f "                     \"")
             (loop for col from 0 below @cols
                   for tile = (aref @game-map row col)
                   for type = (type-of tile)
                   do (case type
                        (land  (princ #\. f))
                        (water (princ #\% f))
                        (food  (princ #\* f))
                        (ant (if (dead tile)
                                 (princ (code-char (+ (pid tile) 65)) f)
                                 (princ (code-char (+ (pid tile) 97)) f)))
                        (otherwise (error "Unknown tile type: ~S (~D,~D)"
                                          tile row col))))
             (if (< (+ row 1) @rows)
                 (format f "\",~%")
                 (format f "\"~%")))
    (format f (mkstr "             ]~%"
                     "        },~%"
                     "        \"ants\": [~%"))
    (loop with food-length = (+ (length @food)
                                (length @contested-food))
          for bot across @bots
          for i from 1
          do (loop for ant in (append (reverse (ants bot))
                                      (reverse (dead-ants bot)))
                   for j from 1
                   do (format f "            [ ~D, ~D, ~D, ~D, ~D, ~D, ~S ]~A~%"
                              (initial-row ant)
                              (initial-col ant)
                              (start-turn ant)
                              (conversion-turn ant)
                              (if (dead ant)
                                  (end-turn ant)
                                  (+ @turns 1))
                              (pid ant)
                              (coerce (orders ant) 'string)
                              (if (or (< i (length @bots))
                                      (< j (+ (length (ants bot))
                                              (length (dead-ants bot))))
                                      (> food-length 0))
                                   ","
                                   ""))))
    (loop with food-length = (+ (length @food)
                                (length @contested-food))
          for food in (append (reverse @food)
                              (reverse @contested-food))
          for i from 1
          do (format f "            [ ~D, ~D, ~D, ~D ]~A~%"
                     (row food)
                     (col food)
                     (start-turn food)
                     (if (= 0 (conversion-turn food))
                         (+ @turns 1)
                         (conversion-turn food))
                     (if (< i food-length) "," "")))
    (format f (mkstr "        ],~%"
                     "        \"scores\": [~%"))
    (loop for bot across @bots
          for i from 1
          do (princ "            [" f)
             (loop for n across (scores bot)
                   for j from 1
                   do (princ n f)
                      (when (< j (length (scores bot)))
                        (princ "," f)))
             (princ "]" f)
             (when (< i (length @bots))
               (princ "," f))
             (terpri f))
    (format f (mkstr "        ]~%"
                     "    },~%"
                     "    \"score\": [~A],~%"
                     "    \"status\": [~A]~%")
            (players-score-string :sep ", ")
            (players-status-string :sep ", " :quotes t))
    (format f (mkstr "}~%"))))
