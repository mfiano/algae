(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.index-allocator
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:alive-p
   #:free
   #:generate
   #:generation
   #:get-index
   #:id
   #:index-count
   #:make-allocator))

(in-package #:net.mfiano.lisp.algae.index-allocator)

(declaim (inline %make-allocator))
(defstruct (allocator
            (:constructor %make-allocator)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  occupied
  (freed nil :type list)
  (index-count 0 :type (and fixnum (integer 0)))
  (warn-size 0 :type (and fixnum (integer 0))))

(declaim (inline make-index))
(defstruct (index
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; Indices have a unique ID, which is a serial number starting from 0 and
  ;; increasing for each new generated index. However, a newly generated index
  ;; will attempt to reclaim a previously used ID that has since been freed.
  (id 0 :type (and fixnum (integer 0)))
  ;; The generation of an index starts at 0, and when an ID for a newly
  ;; generated index is reclaimed from a previously freed index, the generation
  ;; will be incremented. This allows for re-using ID serial numbers while still
  ;; being able to distinguish an index from a different generation.
  (generation 0 :type (and fixnum (integer 0))))

(u:define-printer (allocator stream)
  (format stream "(occupied: ~d, freed: ~d)"
          (index-count allocator)
          (length (freed allocator))))

(defun make-allocator (&key (initial-capacity 128) (warn-size 1000))
  "Construct a new allocator capable of allocating generational indices.
`INITIAL-CAPACITY` is the initial size of the allocator's occupied array. When
an index is generated, this array will first be resized if it is at full
capacity. `WARN-SIZE` specifies how large the allocator's occupied array can be
before a warning is emitted. This warning is purely to inform the user of
potentially excessive allocations. A warning will be emitted every `WARN-SIZE`
allocations. If `WARN-SIZE` is 0, no warnings will be emitted."
  (%make-allocator :occupied (make-array initial-capacity
                                         :adjustable t
                                         :fill-pointer 0)
                   :warn-size warn-size))

(defun alive-p (allocator index)
  "Check if `INDEX` is alive or not in `ALLOCATOR`. An index is considered
  alive if an index with the same ID and generation is currently occupied in
  the allocator. Returns T if `INDEX` is alive, and as a second return value, T
  if `INDEX` is not alive, but a different generation is currently occupied."
  (let ((id (id index))
        (occupied (occupied allocator)))
    (when (>= id (length occupied))
      ;; If we ever hit this branch, it indicates a programming error, or the
      ;; user incorrectly mismatched an index with an allocator other than the
      ;; one it was generated with. Due to the nature of how indices are
      ;; generated and freed, a generated index should never reach this branch,
      ;; as the occupied array's size is never decreased.
      (error "Index ~d out of bounds. You can only free or check if an index ~
              is alive with the allocator that generated that index."
             id))
    ;; Return T if the index is alive. This also returns a second boolean
    ;; return value specifying whether or not an index of a different generation
    ;; is currently alive.
    (u:when-let ((occupied-index (aref occupied id)))
      (if (= (generation index)
             (generation occupied-index))
          ;; Index is alive.
          (values t nil)
          ;; Index is not alive, but another generation of the index is.
          (values nil t)))))

(defun generate (allocator)
  "Generate an index in `ALLOCATOR`. If an index that was previously freed is
  available, the new index will have the same ID, with a generation 1+ the old
  index. If there are no previously freed indices available, a new index is
  generated with the next available ID and a generation of 0. Returns an
  instance of an index object, and as a second return value, a boolean
  specifying whether or not the newly generated index was reclaimed from a
  previously freed index."
  (let ((occupied (occupied allocator)))
    (flet ((reclaim (allocator)
             ;; Attempt to reclaim any previously freed indices.
             (u:when-let ((index (pop (freed allocator))))
               ;; If we reach this point, it means there was an available free
               ;; index that can be reclaimed. To reduce consing, we'll move the
               ;; free index to the allocator's occupied array at the
               ;; appropriate index, and then increment its generation. This is
               ;; equivalent to, but more efficient than creating a new instance
               ;; of an index.
               ;; NOTE: It was already removed from the allocator's free list in
               ;; the call to POP above.
               (incf (generation index))
               (incf (index-count allocator))
               (setf (aref occupied (id index)) index)
               ;; Return the index, and T as a second return value, denoting
               ;; this index was reclaimed from a previously freed index.
               (values index t))))
      (or (reclaim allocator)
          ;; If we didn't reclaim any previously freed indices, go ahead and
          ;; generate a new index and push it to the allocator's occupied array.
          (let ((index (make-index :id (length occupied)))
                (warn-size (warn-size allocator)))
            (vector-push-extend index occupied)
            (incf (index-count allocator))
            ;; When the allocator's WARN-SIZE is a positive non-zero value, emit
            ;; a warning for the user every WARN-SIZE allocations, so they can
            ;; keep tabs on potentially excessive allocations that may indicate
            ;; a problem.
            (when (and (not (zerop warn-size))
                       (zerop (mod (length occupied) warn-size)))
              (warn "Index allocator has reached ~d indices."
                    (length occupied)))
            ;; Return the index, and NIL as a second return value, denoting this
            ;; index was not reclaimed from a previously freed index.
            (values index nil))))))

(defun free (allocator index)
  "Free `INDEX` from `ALLOCATOR`. This removes the index from the allocator's
occupied array and pushes it to the allocator's freed list so that it can be
reclaimed. Returns T if `INDEX` was successfully freed, else NIL."
  (when (and index
             (alive-p allocator index))
    (setf (aref (occupied allocator) (id index)) nil)
    (decf (index-count allocator))
    (push index (freed allocator))
    ;; Return T if the index was freed, else NIL.
    t))

(defun get-index (allocator id)
  "Retrieves the currently occupied index with `ID` from `ALLOCATOR`. Returns
NIL if `ID` does not denote a currently occupied index."
  (let ((occupied (occupied allocator)))
    (when (< id (length occupied))
      (aref occupied id))))
