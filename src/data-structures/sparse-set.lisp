(in-package #:cl-user)

;;;; An implementation of the sparse set data structure.

(defpackage #:net.mfiano.lisp.algae.data-structures.sparse-set
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:length
   #:map)
  (:export
   #:copy
   #:clear
   #:delete
   #:find
   #:insert
   #:length
   #:make-sparse-set
   #:map))

(in-package #:net.mfiano.lisp.algae.data-structures.sparse-set)

(defstruct (sparse-set
            (:constructor %make-sparse-set)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (dense (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (sparse (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (%length 0 :type fixnum))

(u:define-printer (sparse-set stream)
  (format stream "~a" (subseq (dense sparse-set) 0 (%length sparse-set))))

(defun make-sparse-set (&key (size 128))
  (%make-sparse-set :dense (make-array size
                                       :element-type 'fixnum
                                       :initial-element 0)
                    :sparse (make-array size
                                        :element-type 'fixnum
                                        :initial-element most-positive-fixnum)))

(u:fn-> copy (sparse-set) sparse-set)
(defun copy (sparse-set)
  (declare (optimize speed))
  (%make-sparse-set :dense (copy-seq (dense sparse-set))
                    :sparse (copy-seq (sparse sparse-set))
                    :%length (%length sparse-set)))

(u:fn-> insert (sparse-set fixnum) boolean)
(defun insert (sparse-set value)
  (declare (optimize speed))
  (let* ((length (%length sparse-set))
         (dense (dense sparse-set))
         (sparse (sparse sparse-set))
         (a (aref sparse value)))
    (when (or (>= a length)
              (/= (aref dense a) value))
      (setf (aref sparse value) length
            (aref dense length) value)
      (incf (%length sparse-set))
      t)))

(u:fn-> delete (sparse-set fixnum) boolean)
(defun delete (sparse-set value)
  (declare (optimize speed))
  (let* ((length (1- (%length sparse-set)))
         (dense (dense sparse-set))
         (sparse (sparse sparse-set))
         (a (aref sparse value)))
    (when (and (<= a length)
               (= (aref dense a) value))
      (let ((end (aref dense length)))
        (setf (%length sparse-set) length
              (aref dense a) end
              (aref sparse end) a)
        t))))

(u:fn-> find (sparse-set fixnum) boolean)
(defun find (sparse-set value)
  (declare (optimize speed))
  (let ((a (aref (sparse sparse-set) value)))
    (and (< a (%length sparse-set))
         (= (aref (dense sparse-set) a) value))))

(u:fn-> clear (sparse-set) (values))
(defun clear (sparse-set)
  (declare (optimize speed))
  (setf (%length sparse-set) 0)
  (values))

(u:fn-> length (sparse-set) fixnum)
(u:defun-inline length (sparse-set)
  (declare (optimize speed))
  (%length sparse-set))

(u:fn-> map (sparse-set function) null)
(defun map (sparse-set func)
  (declare (optimize speed))
  (dotimes (i (%length sparse-set))
    (funcall func (aref (dense sparse-set) i))))
