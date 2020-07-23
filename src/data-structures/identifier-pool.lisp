(in-package #:cl-user)

;;;; An identifier pool, for lack of a better name. This package is able to
;;;; generate increasing identifiers (integers) in such a way that previously
;;;; deallocated identifiers are available to be reclaimed by the generator the
;;;; next time one is allocated. Essentially, it solves the "ABA Problem":
;;;; https://en.wikipedia.org/wiki/ABA_problem

;;;; It does so in a space-efficient manner, without the need for the storage of
;;;; a list of deallocated identifiers. It does this by keeping deallocated
;;;; identifiers around and modifying their data on deletion to build a sort of
;;;; implicit linked list. That is, when an identifier is marked for deletion,
;;;; the following occurs:

;;;; * The packed version portion of its data is incremented.

;;;; * The packed ID portion of its data is set to the integer stored in the
;;;;   pool's FREE-HEAD slot (FREE-HEAD can be thought of as the head of the
;;;;   implicit linked list). If FREE-HEAD is null, then instead, all of the
;;;;   bits of the ID are set. #xFFFFF represents the "invalid" ID.

;;;; * FREE-HEAD is set to the ID portion of its data.

;;;; This, in effect, constructs an implicit linked list of the next available
;;;; identifiers that can be generated.

(defpackage #:net.mfiano.lisp.algae.identifier-pool
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:count
   #:map)
  (:export
   #:active-p
   #:count
   #:free
   #:generate
   #:make-pool
   #:map))

(in-package #:net.mfiano.lisp.algae.identifier-pool)

(u:define-constant +id-bits+ 24 :test #'=)

(u:define-constant +id-mask+ (1- (expt 2 +id-bits+)) :test #'=)

(u:define-constant +version-bits+ 32 :test #'=)

(u:define-constant +version-mask+ (1- (expt 2 +version-bits+)) :test #'=)

(declaim (inline %make-pool))
(defstruct (pool
            (:constructor %make-pool)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  store
  free-head
  (count 0 :type u:ub24))

(declaim (inline unpack))
(u:fn-> unpack (fixnum) (values u:ub24 u:ub32))
(defun unpack (identifier)
  (declare (optimize speed))
  (values (ldb (byte +id-bits+ 0) identifier)
          (ldb (byte +version-bits+ +id-bits+) identifier)))

(declaim (inline pack))
(u:fn-> pack (u:ub24 u:ub32) fixnum)
(defun pack (id version)
  (declare (optimize speed))
  (dpb version (byte +version-bits+ +id-bits+)
       (dpb id (byte +id-bits+ 0) 0)))

(defun make-pool (&key (size 128))
  (%make-pool :store (make-array size
                                 :adjustable t
                                 :fill-pointer 0
                                 :element-type 'fixnum)))

(defun generate (pool)
  (let ((store (store pool))
        (free-head (free-head pool)))
    (incf (count pool))
    (if free-head
        (u:mvlet ((id version (unpack (aref store free-head))))
          (setf (free-head pool) (if (= id +id-mask+) nil id)
                (aref store free-head) (pack free-head version)))
        (let ((identifier (pack (length store) 0)))
          (vector-push-extend identifier store)
          identifier))))

(defun free (pool identifier)
  (let ((store (store pool))
        (index (unpack identifier)))
    (when (< index (length store))
      (u:mvlet ((id version (unpack (aref store index))))
        (when (= index id)
          (setf (aref store id) (pack (or (free-head pool) +id-mask+)
                                      (logand (1+ version) +version-mask+))
                (free-head pool) id)
          (decf (count pool))
          t)))))

(defun active-p (pool identifier)
  (let ((store (store pool))
        (index (logand identifier +id-mask+)))
    (and (< index (length store))
         (= (unpack (aref store index)) identifier))))

(defun map (pool func)
  (let ((store (store pool)))
    (if (free-head pool)
        (loop :for i :below (length store)
              :for identifier = (aref store i)
              :when (= (logand identifier +id-mask+) i)
                :do (funcall func identifier))
        (dotimes (i (length store))
          (funcall func (aref store i))))))
