;;;; package.lisp

(defpackage #:trivial-renamer
  (:nicknames :renamer)
  (:use #:alexandria #:cl)
  (:export
   :rules-clear :rules-add :rule-add
   :renamer :rename
   :old->new :new->old)
  )



