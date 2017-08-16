;;;; package.lisp

(defpackage #:trivial-renamer
  (:nicknames :rename)
  (:use #:alexandria #:cl)
  (:export
   :rules :rule
   :renamer :*default-renamer*
   :please
   :reset
   :old->new :new->old :default :normal :one-to-one :cache 
   )
  )



