;;;; play-game.asd

(in-package :cl-user)

(asdf:defsystem :play-game
  :serial t
  :components ((:module src-play-game
                :serial t
                :components ((:file "package")
                             (:file "specials")
                             (:file "classes")
                             (:file "replay")
                             (:file "play-game"))))
  :depends-on (:ants-common :com.dvlsoft.clon :parse-number :split-sequence))
