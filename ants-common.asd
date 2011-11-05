;;;; ants-common.asd

(in-package :cl-user)

(asdf:defsystem :ants-common
  :components ((:module src-common
                :serial t
                :components ((:file "package")
                             (:file "specials")
                             (:file "macros")
                             (:file "classes")
                             (:file "handlers")
                             (:file "predicates")
                             (:file "common")))))
