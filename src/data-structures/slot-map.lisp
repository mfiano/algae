(in-package #:cl-user)

;;;; An implementation of the slot-map data structure.
;;;; Reference 1: https://github.com/WG21-SG14/SG14/blob/master/SG14/slot_map.h
;;;; Reference 2: http://bitsquid.blogspot.com/2011/09/managing-decoupling-part-4-id-lookup.html
;;;; Reference 3: https://github.com/orlp/slotmap

(defpackage #:net.mfiano.lisp.algae.data-structures.slot-map
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find)
  (:export
   #:delete
   #:find
   #:insert
   #:map-keys
   #:map-values))

(in-package #:net.mfiano.lisp.algae.data-structures.slot-map)

(u:define-constant +id-bits+ 24 :test #'=)

(u:define-constant +version-bits+ 32 :test #'=)

(defstruct (slot-map
            (:constructor %make-slot-map)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  data
  slots
  reverse-map
  (free-head nil :type (or null u:ub24))
  (free-tail nil :type (or null u:ub24)))

(u:define-printer (slot-map stream)
  (format stream "~s" (data slot-map)))

(defun make-slot-map (&key (size 128) (data-type 'u:ub32))
  (flet ((vec (type)
           (make-array size
                       :adjustable t
                       :fill-pointer 0
                       :element-type type)))
    (%make-slot-map :data (vec data-type)
                    :slots (vec 'fixnum)
                    :reverse-map (vec 'u:ub24))))

(u:fn-> pack (u:ub24 u:ub32) fixnum)
(u:defun-inline pack (id version)
  (declare (optimize speed))
  (dpb version (byte +version-bits+ +id-bits+)
       (dpb id (byte +id-bits+ 0) 0)))

(u:fn-> unpack (fixnum) (values u:ub24 u:ub32))
(u:defun-inline unpack (packed)
  (declare (optimize speed))
  (values (ldb (byte +id-bits+ 0) packed)
          (ldb (byte +version-bits+ +id-bits+) packed)))

(u:defun-inline id (packed)
  (declare (optimize speed))
  (nth-value 0 (unpack packed)))

(u:defun-inline version (packed)
  (declare (optimize speed))
  (nth-value 1 (unpack packed)))

(defmacro repack-id (place id)
  `(setf ,place (pack ,id (version ,place))))

(u:fn-> enqueue-free (slot-map fixnum) null)
(defun enqueue-free (slot-map key)
  (let ((slots (slots slot-map))
        (tail (free-tail slot-map))
        (id (id key)))
    (repack-id (aref slots id) id)
    (setf (free-tail slot-map) id)
    (if tail
        (repack-id (aref slots tail) id)
        (setf (free-head slot-map) id))
    (values)))

(u:fn-> dequeue-free (slot-map) (or u:ub24 null))
(defun dequeue-free (slot-map)
  (u:when-let* ((head (free-head slot-map))
                (next (id (aref (slots slot-map) head))))
    (if (= head next)
        (setf (free-head slot-map) nil
              (free-tail slot-map) nil)
        (setf (free-head slot-map) next))
    head))

(u:fn-> insert (slot-map t) fixnum)
(defun insert (slot-map value)
  (let* ((data (data slot-map))
         (data-index (length data))
         (slots (slots slot-map))
         (free (dequeue-free slot-map)))
    (vector-push-extend value data)
    (if free
        (let ((version (version (aref slots free))))
          (setf (aref slots free) (pack data-index version))
          (vector-push-extend free (reverse-map slot-map))
          (values (pack free version)))
        (let ((slot-index (length slots)))
          (vector-push-extend (pack data-index 0) slots)
          (vector-push-extend slot-index (reverse-map slot-map))
          (values (pack slot-index 0))))))

(u:fn-> delete (slot-map fixnum) boolean)
(defun delete (slot-map key)
  (u:mvlet* ((id version (unpack key))
             (slots (slots slot-map)))
    (when (< id (length slots))
      (u:mvlet ((slot-id slot-version (unpack (aref slots id))))
        (when (= version slot-version)
          (let* ((data (data slot-map))
                 (reverse-map (reverse-map slot-map))
                 (reverse-index (vector-pop reverse-map)))
            (setf (aref data slot-id) (vector-pop data)
                  (aref reverse-map slot-id) reverse-index
                  (aref slots id) (pack id (1+ (version (aref slots id)))))
            (repack-id (aref slots reverse-index) slot-id)
            (enqueue-free slot-map key)
            t))))))

(u:fn-> find (slot-map fixnum) t)
(defun find (slot-map key)
  (u:mvlet ((id version (unpack key))
            (slots (slots slot-map)))
    (when (< id (length slots))
      (u:mvlet ((slot-id slot-version (unpack (aref slots id))))
        (when (= version slot-version)
          (aref (data slot-map) slot-id))))))

(u:fn-> map-keys (slot-map function) null)
(defun map-keys (slot-map func)
  (dotimes (i (length (data slot-map)))
    (let* ((reverse (aref (reverse-map slot-map) i))
           (slot (aref (slots slot-map) reverse)))
      (funcall func (pack reverse (version slot))))))

(u:fn-> map-values (slot-map function) null)
(defun map-values (slot-map func)
  (map nil func (data slot-map)))
