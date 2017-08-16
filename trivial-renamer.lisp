(in-package #:trivial-renamer)
;;
;; This portion of trivial-renamer deals with creating and updating rules.
;;
(defclass ruler ()
  ((db :accessor db :initarg :db :initform nil)
   (test :accessor test :initarg :test :initform #'equal)
   (overwrite  :accessor overwrite  :initarg :overwrite  :initform t)
   (validate :accessor validate :initarg :validate :initform (lambda (key val r)))))

(defmethod initialize-instance :after ((obj ruler) &key)
  (setf (db obj) (make-hash-table :test (test obj))))

(defun rules-clear(ruler)
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
   (default    :accessor default  :initarg :default      :initform #'default-rename-function)
   (normal     :accessor normal   :initarg :normal       :initform #'default-rename-function)
   ;; database of name mappings
   (old->new   :accessor old->new :initform nil)
   (new->old   :accessor new->old :initform nil)
   (one-to-one :accessor one-to-one :initarg :one-to-one :initform t)
   (cache      :accessor cache    :initarg :cache    :initform t)
   ;; object accessors
))
;; some useful defaults...
(defun default-get-name (obj)
  (copy-seq obj))

(defun default-rename-function (string obj renamer)
  (copy-seq string))
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
  (with-slots (db categorize get-name normal default) renamer
    (let ((rules (gethash (funcall categorize obj) db))
	  (oldname (funcall get-name obj)))
      (if rules
	  (funcall normal (apply-rules rules oldname) obj renamer)
	  (funcall default oldname obj renamer)))))

(defun rename (obj renamer)
  "rename obj and return new name, possibly using cache and guaranteeing 1-1 correspondence"
  (with-slots (new->old old->new db) renamer
    (let ((newname
	   (if old->new ;;if caching, make sure it winds up in the cache.
	       (ensure-gethash obj old->new (rename-for-sure obj renamer))
	       ;;not caching
	       (rename-for-sure obj renamer))))
      ;; if we need a new->old mapping, we should check for uniqueness
      (when new->old
	(let ((existing (gethash newname new->old)))
	  (if existing
	      (unless (equal obj existing)
		(error "renaming ~A to ~A failed: ~A already maps to ~A" obj newname existing newname))
	      (setf (gethash newname new->old) obj))))
      newname)))


;;------------------------------------------------------------------------------
;; rule application
(defun apply-rules (rules name)
  (loop for rule in rules do
       (setf name (ppcre:regex-replace-all (car rule) name (cdr rule))))
  name)
 



(defparameter *test* 
  (make-instance 'renamer::renamer
		 :default (lambda (str obj renamer) (string-downcase str))
		 :categorize (lambda (obj) (subseq obj 0 3))))
  
