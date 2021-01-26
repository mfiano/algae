(in-package #:cl-user)

;;;; This package provides an optimized 1-dimensional array type that can only
;;;; store fixnums, and which is able to automatically grow when pushing new
;;;; elements to the end. It is not very useful in most situations, as it
;;;; doesn't implement the Common Lisp sequences protocol, nor does it conform
;;;; to the usual interface of Common Lisp arrays for PUSH and POP.

(defpackage #:algae.dynamic-array
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:aref
   #:fill-pointer
   #:find
   #:length
   #:make-array
   #:map
   #:pop
   #:push)
  (:export
   #:aref
   #:copy
   #:dynamic-array
   #:find
   #:length
   #:make-array
   #:map
   #:pop
   #:push))

(in-package #:algae.dynamic-array)

(declaim (inline %make-array))
(defstruct (dynamic-array
            (:constructor %make-array)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data (cl:make-array 0 :element-type t) :type simple-vector)
  (initial-element 0)
  (fill-pointer 0 :type fixnum))

(u:define-printer (dynamic-array stream :type nil)
  (format stream "~{~a~^ ~}"
          (coerce (subseq (data dynamic-array) 0 (fill-pointer dynamic-array)) 'list)))

(defun make-array (&key (size 0) (capacity 128) (element-type t)
                     initial-element)
  (assert (<= size capacity))
  (let ((data (cl:make-array capacity :element-type element-type :initial-element initial-element)))
    (%make-array :data data :initial-element initial-element :fill-pointer size)))

(u:fn-> copy (dynamic-array) dynamic-array)
(defun copy (dynamic-array)
  (declare (optimize speed))
  (%make-array :data (u:copy-array (data dynamic-array))
               :initial-element (initial-element dynamic-array)
               :fill-pointer (fill-pointer dynamic-array)))

(u:fn-> length (dynamic-array) fixnum)
(u:defun-inline length (dynamic-array)
  (fill-pointer dynamic-array))

(u:fn-> aref (dynamic-array fixnum) t)
(u:defun-inline aref (dynamic-array index)
  (declare (optimize speed))
  (if (< index (fill-pointer dynamic-array))
      (locally (declare (optimize (safety 0)))
        (cl:aref (data dynamic-array) index))
      (error "Index out of bounds.")))

(u:fn-> (setf aref) (t dynamic-array fixnum) t)
(u:defun-inline (setf aref) (value dynamic-array index)
  (declare (optimize speed))
  (if (< index (fill-pointer dynamic-array))
      (locally (declare (optimize (safety 0)))
        (setf (cl:aref (data dynamic-array) index) value))
      (error "Index out of bounds.")))

(u:fn-> pop (dynamic-array &optional boolean) t)
(u:defun-inline pop (dynamic-array &optional delete-p)
  (declare (optimize speed))
  (let ((data (data dynamic-array))
        (length (fill-pointer dynamic-array)))
    (unless (zerop length)
      (prog1 (cl:aref data (decf (fill-pointer dynamic-array)))
        (when delete-p
          (setf (cl:aref data (1- length)) nil))))))

(u:fn-> push (dynamic-array t) dynamic-array)
(defun push (dynamic-array value)
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

(u:fn-> map (dynamic-array function) null)
(defun map (dynamic-array func)
  (declare (optimize speed))
  (loop :for i :below (length dynamic-array)
        :for element = (aref dynamic-array i)
        :do (funcall func element)))

(u:fn-> find (dynamic-array t &key (:test function) (:key function)) t)
(declaim (inline find))
(defun find (dynamic-array item &key (test #'eql) key)
  (declare (optimize speed))
  (cl:find item (data dynamic-array) :key key :test test))
