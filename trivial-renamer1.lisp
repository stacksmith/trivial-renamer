(in-package #:trivial-renamer)
;;==============================================================================
;;
;;
#||
(defclass storage-alist ()
  ((db :accessor db :initform nil)
   (size :accessor size :initform 0)))

(defmethod clear ((obj storage-alist))
  (with-slots (db size) obj
    (setf db nil
	  size 0)))

(defmethod fetch (key (obj storage-alist))
  (with-slots (db test) obj
    (cdr (assoc key db))))

(defmethod store (key (obj storage-alist) value &key (if-exists :supersede))
  (with-slots (db test) obj
    (let ((old (asoc key db) ))
      (if old
	  (case if-exists
	    (:supersede (setf (cdr old)  )))))
    (cdr (assoc key  (db obj)))))
||#


(defclass ruler ()
  ((db :accessor db :initarg :db :initform nil)
   (test :accessor test :initarg :test :initform #'equal)
   (overwrite  :accessor overwrite  :initarg :overwrite  :initform t)
   (validate :accessor validate :initarg :validate :initform (lambda (key val r)))))

(defun clear (ruler)
  (clrhash (db ruler)))

(defun rule-add (ruler category val)
  "add a rule for key appending or replacing"
  (with-slots (db validate overwrite) ruler
    (and validate
	 (funcall validate category val ruler))
    (setf (gethash category db)
	  (if overwrite
	      val;
	      (concatenate 'list val (gethash category db))))))

(defun rules-add (ruler rules)
  (loop for (category value) on rules by #'cddr do
       (rule-add ruler  category value)))




(defclass renamer (ruler)
  ((categorize :accessor categorize :initarg :categorize :initform #'identity)
   (get-name   :accessor get-name :initarg :get-name     :initform #'default-get-name)
   (default    :accessor default  :initarg :default      :initform #'default-get-name)
   (normal     :accessor normal   :initarg :normal       :initform #'default-get-name)
   ;; database of name mappings
   (old->new   :accessor old->new :initform nil)
   (new->old   :accessor new->old :initform nil)
   (one-to-one :accessor one-to-one :initarg :one-to-one :initform t)
   (cache      :accessor cache    :initarg :cache    :initform t)
   ;; object accessors
))

(defun default-get-name (obj)
  (copy-seq obj))
;; one-on-one requires us to check for uniqueness of name transformation;
;; a hashtable is a pretty good way to do that.  Mapping new->old is also
;; only valid if one-on-one is on...
;;
;; similarly, if cache is on, we shall keep a table of old->new mappings.
(defmethod initialize-instance :after ((renamer renamer) &key)
  (with-slots (default old->new new->old one-to-one cache) renamer
    (when one-to-one
      (setf new->old (make-hash-table :test 'equal)))
    (when cache
      (setf old->new (make-hash-table :test 'equal)))
    (unless default
      (setf default (lambda (obj) (get-name obj))))))

(defun rename-for-sure (obj renamer)
  (with-slots (db categorize get-name) renamer
    (let ((rules (gethash (funcall categorize obj) db))
	  (oldname (get-name obj)))
      (if rules
	  (normal (apply-rules rules oldname))
	  (default oldname )))))

(defun rename (obj renamer)
  (with-slots (old->new db) renamer
    (let ((newname
	   (if old->new ;;if caching, make sure it winds up in the cache.
	       (ensure-gethash obj old->new (rename-for-sure obj renamer))
	       ;;not caching
	       (rename-for-sure obj renamer))))
      (and new->old
	   )
      )
    ))


;;------------------------------------------------------------------------------
;; rule application
(defun apply-rules (rules name)
  (loop for rule in rules do
       (setf name (ppcre:regex-replace-all (car rule) name (cdr rule))))
  name)
 
