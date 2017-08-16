(in-package #:trivial-renamer)

(defparameter *default-renamer* nil)

;;
;; This portion of trivial-renamer deals with creating and updating rules.
;;
(defclass ruler ()
  ((db :accessor db :initarg :db :initform nil)
   (test :accessor test :initarg :test :initform #'equal)
   (overwrite  :accessor overwrite  :initarg :overwrite  :initform t)
   (validate :accessor validate :initarg :validate
	     :initform (lambda (key val r)
			 (declare (ignore key val r))))))

(defmethod initialize-instance :after ((obj ruler) &key)
  (setf (db obj) (make-hash-table :test (test obj))))

(defun rules-clear(ruler)
  (clrhash (db ruler)))

(defun rule (category val &optional (ruler *default-renamer*))
  "add a rule for key appending or replacing"
  (with-slots (db validate overwrite) ruler
    (and validate
	 (funcall validate category val ruler))
    (setf (gethash category db)
	  (if overwrite
	      val;
	      (concatenate 'list val (gethash category db)))))
  :OK)

(defun rules (rules &optional (ruler *default-renamer*))
  "Add a bunch of rules"
  (loop for i from 0
     for (category value) on rules by #'cddr do
       (rule  category value ruler)
     finally (return i)))


(defclass renamer (ruler)
  (;;(categorize :accessor categorize :initarg :categorize :initform #'identity)
   ;;(get-name   :accessor get-name :initarg :get-name     :initform #'default-get-name)
   (default    :accessor default  :initarg :default      :initform #'default-rename-function)
   (normal     :accessor normal   :initarg :normal       :initform #'default-rename-function)
   (transform  :accessor transform :initarg :transform   :initform #'default-transform)
   ;; database of name mappings
   (old->new   :accessor old->new :initform nil)
   (new->old   :accessor new->old :initform nil)
   (one-to-one :accessor one-to-one :initarg :one-to-one :initform t)
   (cache      :accessor cache    :initarg :cache    :initform t)
   ;; object accessors
))
;; some useful defaults...
(defun default-rename-function (string renamer)
  (declare (ignore renamer))
  (string-downcase string))
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
      (setf old->new (make-hash-table :test 'equal))))
  (setf *default-renamer* renamer))

;;------------------------------------------------------------------------------
;; rule application
(defun default-transform (oldname category rules renamer)
  (declare (ignore category renamer))
  (loop for rule in rules do
       (setf oldname (ppcre:regex-replace-all (car rule) oldname (cdr rule))))
  oldname)

(defun rename-for-sure (oldname category renamer)
  (with-slots (db normal default transform) renamer
    (let ((rules (gethash category db)))
      (if rules
	  (funcall normal
		   (funcall transform oldname category rules renamer)
		   renamer)
	  (funcall default oldname renamer)))))

(defun please (oldname &optional (category t) (renamer *default-renamer*))
  "rename obj and return new name, possibly using cache and guaranteeing 1-1 correspondence"
  (with-slots (new->old old->new db) renamer
    (let ((newname
	   (if old->new ;;cache on?
	       (ensure-gethash oldname old->new
			       (rename-for-sure oldname category renamer))
	       (rename-for-sure oldname category renamer))))
      ;; if we need a new->old mapping, we should check for uniqueness
      (when new->old
	(let ((existing (gethash newname new->old)));already have newname
	  (if existing
	      (unless (equal oldname existing)
		(error "renaming ~A to ~A failed: ~A already maps to ~A"
		       oldname newname existing newname))
	      (setf (gethash newname new->old) oldname))))
      newname)))



(defun reset (&optional (renamer *default-renamer*))
  (with-slots (new->old old->new db) renamer
    (rules-clear renamer)
    (when new->old (clrhash new->old))
    (when old->new (clrhash old->new)))
  renamer)




 
