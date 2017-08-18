;;;; package.lisp

(defpackage #:trivial-renamer
  (:nicknames :rename)
  (:use #:cl)
  (:export
   :rules :rule
   :renamer :*default-renamer*
   :please
   :reset :clear
   :old->new :new->old :default :normal :one-to-one :cache 
   )
  )



