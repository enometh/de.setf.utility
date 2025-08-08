;;; -*- Package: de.setf.utility.implementation; -*-
;; ;madhu 250809 provide lock implementation via bordeaux-threads, see lock.lisp

(in-package :de.setf.utility.implementation)

;; (require 'bordeaux-threads)

(modpackage :de.setf.utility.lock
  (:use )
  (:nicknames :setf.lock)
  (:export
   :call-with-object-locked
   :instance-lock
   :with-object-locked
   :synchronized-object
   :run-in-thread
   ))

(defmacro setf.lock:with-object-locked ((object) &rest body)
  (let* ((function (gensym)))
    `(flet ((,function () ,@body))
       (declare (dynamic-extent #',function))
       (setf.lock:call-with-object-locked #',function ,object))))


(defun setf.lock:call-with-object-locked (function instance)
  "invoke the argument function while holding the lock on the argument instance."
  (bordeaux-threads:with-lock-held ((setf.lock:instance-lock instance))
    (funcall function)))

(defvar *instance-locks* (make-weak-hash-table))

;; make a lock for the registry itself
(setf (gethash *instance-locks* *instance-locks*) (bordeaux-threads:make-lock "setf.de lock"))

(defclass setf.lock:synchronized-object ()
  ((lock
    ;; :initform nil
    :initarg :lock
    :reader get-instance-lock))
  (:documentation
   "a synchronized-object binds a lock for use with the synchronized and call-with-instance-lock-held.
    if no lock is provided as an initialization argument, one with be created upon first reference."))

(defGeneric setf.lock:instance-lock (instance)
  (:method ((datum t))
           (flet ((new-instance-lock ()
                    (or (gethash datum *instance-locks*)
                        (setf (gethash datum *instance-locks*) (bordeaux-threads:make-lock "setf.de instance")))))
             (declare (dynamic-extent #'new-instance-lock))
             (or (gethash datum *instance-locks*)
                 (setf.lock:call-with-object-locked #'new-instance-lock *instance-locks*))))
  (:documentation
   "retrieve an instance's lock. if the instance is not a synchronized object manage a lock for it in a central,
    weak hashtable."))

(defmethod setf.lock:instance-lock ((instance setf.lock:synchronized-object))
  "generate and bind an instance lock upon demand only."
  (if (slot-boundp instance 'lock)
    (get-instance-lock instance)
    (setf (slot-value instance 'lock) (bordeaux-threads:make-lock "setf.de instance"))))

(defun setf.lock:run-in-thread (function
                   &key (name "anonymous") (priority 0)
                   parameters)
  "Runs function in it's own thread."
  (declare (ignore priority))
  (flet ((run-op () (apply function parameters)))
      (bordeaux-threads:make-thread  #'run-op :name name)))
