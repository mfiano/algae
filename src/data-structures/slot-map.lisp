(in-package #:cl-user)

;;;; An implementation of the slot-map data structure.
;;;; Reference 1: https://github.com/WG21-SG14/SG14/blob/master/SG14/slot_map.h
;;;; Reference 2: http://bitsquid.blogspot.com/2011/09/managing-decoupling-part-4-id-lookup.html
;;;; Reference 3: https://github.com/orlp/slotmap

(defpackage #:net.mfiano.lisp.algae.data-structures.slot-map
  (:local-nicknames
   (#:da #:net.mfiano.lisp.algae.data-structures.dynamic-array)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find)
  (:export
   #:delete
   #:find
   #:insert
   #:make-slot-map
   #:map-keys
   #:map-values))

(in-package #:net.mfiano.lisp.algae.data-structures.slot-map)

(u:define-constant +id-bits+ 24 :test #'=)

(u:define-constant +version-bits+ 32 :test #'=)

(declaim (inline make-slot-map))
(defstruct (slot-map
            (:constructor %make-slot-map)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (slots (da:make-array) :type da:dynamic-array)
  (reverse-map (da:make-array) :type da:dynamic-array)
  (free-head nil :type (or null u:ub24))
  (free-tail nil :type (or null u:ub24)))

(u:define-printer (slot-map stream)
  (format stream "~s" (data slot-map)))

(defun make-slot-map (&key (capacity 128) (initial-element 0)
                        (data-type 'u:ub32))
  (let ((data (make-array capacity
                          :adjustable t
                          :fill-pointer 0
                          :initial-element initial-element
                          :element-type data-type)))
    (%make-slot-map :data data
                    :slots (da:make-array :capacity capacity)
                    :reverse-map (da:make-array :capacity capacity))))

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
(u:defun-inline enqueue-free (slot-map key)
  (declare (optimize speed))
  (let ((slots (slots slot-map))
        (tail (free-tail slot-map))
        (id (id key)))
    (repack-id (da:aref slots id) id)
    (setf (free-tail slot-map) id)
    (if tail
        (repack-id (da:aref slots tail) id)
        (setf (free-head slot-map) id))
    (values)))

(u:fn-> dequeue-free (slot-map) (or u:ub24 null))
(u:defun-inline dequeue-free (slot-map)
  (declare (optimize speed))
  (u:when-let* ((head (free-head slot-map))
                (next (id (da:aref (slots slot-map) head))))
    (if (= head next)
        (setf (free-head slot-map) nil
              (free-tail slot-map) nil)
        (setf (free-head slot-map) next))
    head))

(u:fn-> insert (slot-map t) fixnum)
(defun insert (slot-map value)
  (declare (optimize speed))
  (let* ((data (data slot-map))
         (data-index (length data))
         (slots (slots slot-map))
         (free (dequeue-free slot-map)))
    (vector-push-extend value data)
    (if free
        (let ((version (version (da:aref slots free))))
          (setf (da:aref slots free) (pack data-index version))
          (da:push (reverse-map slot-map) free)
          (values (pack free version)))
        (let ((slot-index (da:length slots)))
          (da:push slots (pack data-index 0))
          (da:push (reverse-map slot-map) slot-index)
          (values (pack slot-index 0))))))

(u:fn-> delete (slot-map fixnum) boolean)
(defun delete (slot-map key)
  (declare (optimize speed))
  (u:mvlet* ((id version (unpack key))
             (slots (slots slot-map)))
    (when (< id (da:length slots))
      (u:mvlet ((slot-id slot-version (unpack (da:aref slots id))))
        (when (= version slot-version)
          (let* ((data (data slot-map))
                 (reverse-map (reverse-map slot-map))
                 (reverse-index (da:pop reverse-map)))
            (locally (declare (optimize (speed 1)))
              (setf (aref data slot-id) (vector-pop data)))
            (setf (da:aref reverse-map slot-id) reverse-index
                  (da:aref slots id) (pack id
                                           (1+ (version (da:aref slots id)))))
            (repack-id (da:aref slots reverse-index) slot-id)
            (enqueue-free slot-map key)
            t))))))

(u:fn-> find (slot-map fixnum) t)
(defun find (slot-map key)
  (u:mvlet ((id version (unpack key))
            (slots (slots slot-map)))
    (when (< id (da:length slots))
      (u:mvlet ((slot-id slot-version (unpack (da:aref slots id))))
        (when (= version slot-version)
          (aref (data slot-map) slot-id))))))

(u:fn-> map-keys (slot-map function) null)
(defun map-keys (slot-map func)
  (dotimes (i (length (data slot-map)))
    (let* ((reverse (da:aref (reverse-map slot-map) i))
           (slot (da:aref (slots slot-map) reverse)))
      (funcall func (pack reverse (version slot))))))

(u:fn-> map-values (slot-map function) null)
(defun map-values (slot-map func)
  (let ((data (data slot-map)))
    (dotimes (i (length data))
      (funcall func (aref data i)))))
