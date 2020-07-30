(in-package #:cl-user)

;;;; This package provides an optimized 1-dimensional array type that can only
;;;; store fixnums, and which is able to automatically grow when pushing new
;;;; elements to the end. It is not very useful in most situations, as it
;;;; doesn't implement the Common Lisp sequences protocol, nor does it conform
;;;; to the usual interface of Common Lisp arrays for PUSH and POP.

(defpackage #:net.mfiano.lisp.algae.data-structures.dynamic-array
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:aref
   #:fill-pointer
   #:length
   #:make-array
   #:pop
   #:push)
  (:export
   #:aref
   #:dynamic-array
   #:length
   #:make-array
   #:pop
   #:push))

(in-package #:net.mfiano.lisp.algae.data-structures.dynamic-array)

(declaim (inline data fill-pointer))
(defstruct (dynamic-array
            (:constructor %make-array)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data (cl:make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (initial-element 0 :type fixnum)
  (fill-pointer 0 :type fixnum))

(u:define-printer (dynamic-array stream :type nil)
  (format stream "~a"
          (subseq (data dynamic-array) 0 (fill-pointer dynamic-array))))

(defun make-array (&key (size 0) (capacity 128) (initial-element 0))
  (assert (<= size capacity))
  (let ((data (cl:make-array capacity
                             :element-type 'fixnum
                             :initial-element initial-element)))
    (%make-array :data data
                 :initial-element initial-element
                 :fill-pointer size)))

(u:fn-> length (dynamic-array) fixnum)
(u:defun-inline length (dynamic-array)
  (fill-pointer dynamic-array))

(u:fn-> aref (dynamic-array fixnum) fixnum)
(u:defun-inline aref (dynamic-array index)
  (declare (optimize speed (safety 0)))
  (cl:aref (data dynamic-array) index))

(u:fn-> (setf aref) (fixnum dynamic-array fixnum) fixnum)
(u:defun-inline (setf aref) (value dynamic-array index)
  (declare (optimize speed (safety 0)))
  (setf (cl:aref (data dynamic-array) index) value))

(u:fn-> pop (dynamic-array) fixnum)
(u:defun-inline pop (dynamic-array)
  (declare (optimize speed))
  (if (zerop (fill-pointer dynamic-array))
      (error "No elements to pop.")
      (locally (declare (optimize (safety 0)))
        (cl:aref (data dynamic-array)
                 (decf (fill-pointer dynamic-array))))))

(u:fn-> push (dynamic-array fixnum) dynamic-array)
(u:defun-inline push (dynamic-array value)
  (declare (optimize speed))
  (let* ((data (data dynamic-array))
         (length (cl:length data))
         (fill-pointer (fill-pointer dynamic-array)))
    (when (= fill-pointer length)
      (let ((data (adjust-array
                   data
                   (max 16 (min (* length 2) most-positive-fixnum))
                   :initial-element (initial-element dynamic-array))))
        (setf (data dynamic-array) data)))
    (locally (declare (optimize (safety 0)))
      (setf (cl:aref (data dynamic-array) fill-pointer) value)
      (incf (fill-pointer dynamic-array)))
    dynamic-array))
