(in-package #:cl-user)

;;;; An identifier allocator, for lack of a better name. This has not been fully
;;;; thought out yet. Do not use. Instead, check out identifier-pool-v2, which
;;;; works and is more efficient.

(defpackage #:net.mfiano.lisp.algae.identifier-pool.v1
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:count
   #:find)
  (:export
   #:active-p
   #:count
   #:find
   #:free
   #:generate
   #:id
   #:make-pool
   #:version))

(in-package #:net.mfiano.lisp.algae.identifier-pool.v1)

(declaim (inline %make-pool))
(defstruct (pool
            (:constructor %make-pool)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  active
  (freed nil :type list)
  (count 0 :type (and fixnum (integer 0)))
  (warn-size 0 :type (and fixnum (integer 0))))

(declaim (inline make-identifier))
(defstruct (identifier
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; Identifiers have a unique ID, which is a serial number starting from 0 and
  ;; increasing for each new generated identifier. However, a newly generated
  ;; identifier will attempt to reclaim a previously used ID that has since been
  ;; freed.
  (id 0 :type (and fixnum (integer 0)))
  ;; The version of an identifier starts at 0, and when an ID for a newly
  ;; generated identifier is reclaimed from a previously freed identifier, the
  ;; version will be incremented. This allows for re-using ID serial numbers
  ;; while still being able to distinguish an identifier from a different
  ;; version.
  (version 0 :type (and fixnum (integer 0))))

(u:define-printer (pool stream)
  (format stream "(active: ~d, freed: ~d)"
          (count pool)
          (length (freed pool))))

(defun make-pool (&key (size 128) (warn-size 1000))
  "Construct a new pool capable of generating generational identifiers. `SIZE`
is the initial size of the pool's active array. When an identifier is generated,
this array will first be resized if it is at full capacity. `WARN-SIZE`
specifies how large the pool's active array can be before a warning is emitted.
This warning is purely to inform the user of potentially excessive allocations.
A warning will be emitted every `WARN-SIZE` allocations. If `WARN-SIZE` is 0, no
warnings will be emitted."
  (%make-pool :active (make-array size :adjustable t :fill-pointer 0)
              :warn-size warn-size))

(defun active-p (pool identifier)
  "Check if `IDENTIFIER` is active or not in `POOL`. An identifier is considered
  active if an identifier with the same ID and version is currently in the pool.
  Returns T if `IDENTIFIER` is active, and as a second return value, T if
  `IDENTIFIER` is not active, but a different version is currently active."
  (let ((id (id identifier))
        (active (active pool)))
    (when (>= id (length active))
      ;; If we ever hit this branch, it indicates a programming error, or the
      ;; user incorrectly mismatched an identifier with a pool other than the
      ;; one it was generated with. Due to the nature of how identifiers are
      ;; generated and freed, we should never reach this branch, as the active
      ;; array's size is never decreased.
      (error "ID ~d out of bounds. You can only free or check if an identifier ~
              is active with the pool that generated it."
             id))
    ;; Return T if the identifier is active. This also returns a second boolean
    ;; return value specifying whether or not an identifier of a different
    ;; version is currently active.
    (u:when-let ((active-identifier (aref active id)))
      (if (= (version identifier)
             (version active-identifier))
          ;; Identifier is active.
          (values t nil)
          ;; Identifier is not active, but another version of the identifier is.
          (values nil t)))))

(defun generate (pool)
  "Generate an identifier in `POOL`. If an identifier that was previously freed
  is available, the new identifier will have the same ID, with a version 1+ the
  old identifier. If there are no previously freed identifiers available, a new
  identifier is generated with the next available ID and a version of 0. Returns
  an instance of an identifier object, and as a second return value, a boolean
  specifying whether or not the newly generated identifier was reclaimed from a
  previously freed identifier."
  (let ((active (active pool)))
    (flet ((reclaim (pool)
             ;; Attempt to reclaim any previously freed indices.
             (u:when-let ((identifier (pop (freed pool))))
               ;; If we reach this point, it means there was an available free
               ;; identifier that can be reclaimed. To reduce consing, we'll
               ;; move the free identifier to the pool's active array at the
               ;; appropriate index, and then increment its version. This is
               ;; equivalent to, but more efficient than creating a new instance
               ;; of an identifier. NOTE: It was already removed from the pool's
               ;; free list in the call to POP above.
               (incf (version identifier))
               (incf (count pool))
               (setf (aref active (id identifier)) identifier)
               ;; Return the identifier, and T as a second return value,
               ;; denoting this identifier was reclaimed from a previously freed
               ;; identifier.
               (values identifier t))))
      (or (reclaim pool)
          ;; If we didn't reclaim any previously freed identifier, go ahead and
          ;; generate a new identifier and push it to the pool's active array.
          (let ((identifier (make-identifier :id (length active)))
                (warn-size (warn-size pool)))
            (vector-push-extend identifier active)
            (incf (count pool))
            ;; When the pool's WARN-SIZE is a positive non-zero value, emit a
            ;; warning for the user every WARN-SIZE allocations, so they can
            ;; keep tabs on potentially excessive allocations that may indicate
            ;; a problem.
            (when (and (not (zerop warn-size))
                       (zerop (mod (length active) warn-size)))
              (warn "Pool has reached ~d identifiers."
                    (length active)))
            ;; Return the identifier, and NIL as a second return value, denoting
            ;; this identifier was not reclaimed from a previously freed
            ;; identifier.
            (values identifier nil))))))

(defun free (pool identifier)
  "Free `IDENTIFIER` from `POOL`. This removes the identifier from the pool's
active array and pushes it to the pool's freed list so that it can be reclaimed.
Returns T if `IDENTIFIER` was successfully freed, else NIL."
  (when (and identifier
             (active-p pool identifier))
    (setf (aref (active pool) (id identifier)) nil)
    (decf (count pool))
    (push identifier (freed pool))
    ;; Return T if the identifier was freed, else NIL.
    t))

(defun find (pool id)
  "Retrieves the current active identifier with `ID` from `POOL`. Returns NIL if
`ID` does not denote an active identifier."
  (let ((active (active pool)))
    (when (< id (length active))
      (aref active id))))
