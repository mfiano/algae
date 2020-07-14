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
;;;;   pool's NEXT slot (NEXT can be thought of as the head of the implicit
;;;;   linked list). If NEXT is null, then instead, the all the bits of the ID
;;;;   are set. #xFFFFF represents the "invalid" ID.

;;;; * NEXT is set to the ID portion of its data.

;;;; This, in effect, constructs an implicit linked list of the next available
;;;; identifiers that can be generated.

;;;; This pool allows for generating about 1 million identifiers (20 bits of a
;;;; ub32 is allocated for the ID portion), and 4096 different versions of the
;;;; same identifier (the remaining 12 bits). These restrictions may be
;;;; customizable in the future.

(defpackage #:net.mfiano.lisp.algae.identifier-pool.v2
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:map)
  (:export
   #:active-p
   #:free
   #:generate
   #:make-pool
   #:map))

(in-package #:net.mfiano.lisp.algae.identifier-pool.v2)

(u:define-constant +id-bits+ 20 :test #'=)

(u:define-constant +id-mask+ (1- (expt 2 +id-bits+)) :test #'=)

(u:define-constant +version-bits+ 12 :test #'=)

(u:define-constant +version-mask+ (1- (expt 2 +version-bits+)) :test #'=)

(declaim (inline %make-pool))
(defstruct (pool
            (:constructor %make-pool)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  store
  next)

(declaim (inline unpack))
(u:fn-> unpack (u:ub32) (values u:ub32 u:ub32))
(defun unpack (identifier)
  (declare (optimize speed))
  (values (ldb (byte +id-bits+ 0) identifier)
          (ldb (byte +version-bits+ +id-bits+) identifier)))

(declaim (inline pack))
(u:fn-> pack (u:ub32 u:ub32) u:ub32)
(defun pack (id version)
  (declare (optimize speed))
  (dpb version (byte +version-bits+ +id-bits+)
       (dpb id (byte +id-bits+ 0) 0)))

(defun make-pool (&key (size 128))
  (%make-pool :store (make-array size
                                 :adjustable t
                                 :fill-pointer 0
                                 :element-type 'u:ub32)))

(defun generate (pool)
  (let ((store (store pool))
        (next (next pool)))
    (if next
        (u:mvlet ((id version (unpack (aref store next))))
          (if (= id +id-mask+)
              (setf (next pool) nil)
              (setf (next pool) id))
          (setf (aref store next) (pack next version)))
        (let ((identifier (pack (length store) 0)))
          (vector-push-extend identifier store)
          identifier))))

(defun free (pool identifier)
  (let* ((store (store pool))
         (id (unpack identifier))
         (version (nth-value 1 (unpack (aref store id)))))
    (setf (aref store id) (pack (or (next pool) +id-mask+)
                                (logand (1+ version) +version-mask+))
          (next pool) id)
    (values)))

(defun active-p (pool identifier)
  (let ((store (store pool))
        (index (logand identifier +id-mask+)))
    (and (< index (length store))
         (= (unpack (aref store index)) identifier))))

(defun map (pool func)
  (let ((store (store pool)))
    (if (next pool)
        (loop :for i :below (length store)
              :for identifier = (aref store i)
              :when (= (logand identifier +id-mask+) i)
                :do (funcall func identifier))
        (dotimes (i (length store))
          (funcall func (aref store i))))))
