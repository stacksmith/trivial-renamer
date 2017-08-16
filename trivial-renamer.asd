;;
;;
(asdf:defsystem #:trivial-renamer
  :description "set me!"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (:alexandria :cl-ppcre)
  :components ((:file "package")
	       (:file "trivial-renamer")))

