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
   (new->old   :accessor new->old :initform nil)))

(defmethod initialize-instance :after ((renamer renamer) &key)
  (with-slots (default old->new new->old) renamer
    (setf new->old (make-hash-table :test 'equal))
    (setf old->new (make-hash-table :test 'equal)))
  (setf *default-renamer* renamer))

(defmethod print-object ((object renamer) stream)
  (print-unreadable-object (object stream :type t)
    (format stream " with ~A rules, and ~A names"
	    (hash-table-count (db object))
	    (hash-table-count (old->new object)))))

;; some useful defaults...

(defun default-rename-function (string renamer)
  (declare (ignore renamer))
  (string-downcase string))



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
  "rename obj and return new name"
  (with-slots (new->old old->new db) renamer
    (let (;; propose a name based on current transformation
	  (proposed-name (rename-for-sure oldname category renamer))
	  ;; maybe we've done this before...
	  (stored-name (gethash oldname old->new)))
      (if stored-name
	  (progn
	    (unless (equal stored-name proposed-name)
	      (error "rename: ~a to ~A;
but ~A is already renamed to ~A" oldname proposed-name
oldname stored-name))
	    stored-name)
	  ;; new!
	  ;; check that nothing else maps to proposed-name
	  (let ((reverse (gethash proposed-name new->old)))
	    (when reverse
	      (error "rename: ~A to ~A;
but ~A is already renamed to ~A" oldname proposed-name
reverse proposed-name))
	    ;; Safe to write
	    (setf (gethash proposed-name new->old) oldname
		  (gethash oldname old->new) proposed-name))))))

(defun clear (&optional (renamer *default-renamer*))
  "clear the caches, leaving rules intact"
  (with-slots (new->old old->new) renamer
    (clrhash new->old)
    (clrhash old->new))
  renamer)

(defun reset (&optional (renamer *default-renamer*))
  (with-slots (new->old old->new db) renamer
    (rules-clear renamer)
    (clear renamer))
  renamer)




 
