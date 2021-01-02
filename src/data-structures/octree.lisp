(in-package #:cl-user)

;;;; An implementation of the octree data structure.
;;;; This was an absolute pain to implement, and resulted in a lot of hacky
;;;; code, but appears to work under all the tests that were ran against it.

(defpackage #:algae.octree
  (:local-nicknames
   (#:da #:algae.dynamic-array)
   (#:p3 #:origin.geometry.point3d)
   (#:sphere #:origin.geometry.sphere)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3))
  (:use #:cl)
  (:shadow
   #:count
   #:search)
  (:export
   #:build
   #:center
   #:children
   #:extent
   #:make-octree
   #:octants-neighbor-p
   #:parent
   #:points
   #:query
   #:rank
   #:search))

(in-package #:algae.octree)

(deftype octant-id () 'u:ub32)
(deftype query-result () '(member :disjoint :overlaps :contains :inside))

(declaim (inline %make-octant))
(defstruct (octant
            (:constructor %make-octant)
            (:predicate nil)
            (:copier nil)
            (:conc-name nil))
  (parent nil :type (or octant-id null))
  (children (da:make-array :capacity 8) :type da:dynamic-array)
  (center (p3:point) :type p3:point)
  (extent 0.0 :type u:f32)
  (rank 0 :type u:ub32)
  (point-indices (da:make-array :capacity 32) :type da:dynamic-array))

(declaim (inline %make-octree))
(defstruct (octree
            (:constructor %make-octree)
            (:predicate nil)
            (:copier nil)
            (:conc-name nil))
  (octants (da:make-array :capacity 32) :type da:dynamic-array)
  (points (vector) :type simple-vector))

(u:define-printer (octant stream)
  (format stream "OCTANT (~d children, ~d points)"
          (da:length (children octant))
          (da:length (point-indices octant))))

(u:define-printer (octree stream)
  (format stream "OCTREE (~d octants, ~d points)"
          (da:length (octants octree))
          (length (points octree))))

(u:fn-> make-octant (u:f32) octant)
(defun make-octant (extent)
  (%make-octant :extent extent))

(u:fn-> make-root-octant (simple-vector) octant)
(defun make-root-octant (points)
  (declare (optimize speed))
  (u:mvlet ((point-count (length points))
            (min max (p3:find-min-max points)))
    (v3:with-components ((w (v3:- max min)))
      (let ((octant (make-octant (* (max wx wy wz) 0.5))))
        (dotimes (i point-count)
          (da:push (point-indices octant) i))
        (setf (center octant) (v3:scale (v3:+ max min) 0.5))
        octant))))

(u:fn-> octants-neighbor-p (octant octant) boolean)
(defun octants-neighbor-p (octant1 octant2)
  (declare (optimize speed))
  (let ((e (+ (extent octant1) (extent octant2)))
        (c1 (center octant1))
        (c2 (center octant2)))
    (dotimes (i 3)
      (when (plusp (- (abs (- (aref c2 i) (aref c1 i))) e))
        (return-from octants-neighbor-p nil)))
    t))

(u:fn-> get-octant-cell-index (p3:point) u:ub8)
(declaim (inline get-octant-cell-index))
(defun get-octant-cell-index (point)
  (declare (optimize speed))
  (+ (if (plusp (p3:x point)) 0 1)
     (if (plusp (p3:y point)) 0 2)
     (if (plusp (p3:z point)) 0 4)))

(u:fn-> get-octant-cell-factor (u:ub8) p3:point)
(declaim (inline get-octant-cell-factor))
(defun get-octant-cell-factor (index)
  (declare (optimize speed))
  (p3:point (if (zerop (logand index 1)) 1.0 -1.0)
            (if (zerop (ash (logand index 2) -1)) 1.0 -1.0)
            (if (zerop (ash (logand index 4) -2)) 1.0 -1.0)))

(u:fn-> find-octant (octree octant-id) octant)
(declaim (inline find-octant))
(defun find-octant (octree octant-id)
  (declare (optimize speed))
  (da:aref (octants octree) octant-id))

(u:fn-> make-octree (simple-vector) octree)
(defun make-octree (points)
  (declare (optimize speed))
  (let ((root (make-root-octant points))
        (octree (%make-octree :points points)))
    (da:push (octants octree) root)
    octree))

(u:fn-> insert (octree octant) octant-id)
(declaim (inline insert))
(defun insert (octree octant)
  (declare (optimize speed))
  (let* ((octants (octants octree))
         (id (da:length octants)))
    (da:push octants octant)
    id))

(u:fn-> insert-child (octree octant-id octant) octant-id)
(defun insert-child (octree parent-id child)
  (declare (optimize speed))
  (let* ((id (insert octree child))
         (parent (find-octant octree parent-id))
         (parent-children (children parent)))
    (setf (parent child) parent-id
          (rank child) (da:length parent-children))
    (da:push parent-children id)
    id))

(u:fn-> create-children (octant simple-vector) simple-vector)
(defun create-children (octant points)
  (declare (optimize speed))
  (let ((extent (* (extent octant) 0.5))
        (center (center octant))
        (children (make-array 8))
        (point-indices (point-indices octant)))
    (dotimes (i 8)
      (let ((child (make-octant extent))
            (factors (get-octant-cell-factor i)))
        (v3:+! (center child) (v3:scale factors extent) center)
        (setf (aref children i) child)))
    (when (> (da:length point-indices) 1)
      (da:map
       point-indices
       (lambda (x)
         (let ((index (get-octant-cell-index (v3:- (aref points x) center))))
           (da:push (point-indices (aref children index)) x)))))
    children))

(u:fn-> split-octant (octree octant-id) null)
(declaim (inline split-octant))
(defun split-octant (octree parent-id)
  (declare (optimize speed))
  (let* ((parent (find-octant octree parent-id))
         (children (create-children parent (points octree))))
    (map nil (lambda (x) (insert-child octree parent-id x)) children)))

(u:fn-> count (octree) u:ub32)
(declaim (inline count))
(defun count (octree)
  (declare (optimize speed))
  (da:length (octants octree)))

(u:fn-> build (octree u:ub32) null)
(defun build (octree bucket-size)
  (declare (optimize speed))
  (when (zerop bucket-size)
    (error "Bucket size must not be 0."))
  (when (> (length (points octree)) bucket-size)
    (let ((to-split (list 0)))
      (u:while to-split
        (let ((remaining nil))
          (dolist (parent-id to-split)
            (split-octant octree parent-id)
            (da:map (children (find-octant octree parent-id))
                    (lambda (x)
                      (let ((octant (find-octant octree x)))
                        (when (> (da:length (point-indices octant)) bucket-size)
                          (push x remaining))))))
          (setf to-split (nreverse remaining)))))))

(u:fn-> query (octant sphere:sphere) query-result)
(defun query (octant sphere)
  (declare (optimize speed))
  (let* ((e (extent octant))
         (c (v3:abs (v3:- (sphere:origin sphere) (center octant))))
         (r (sphere:radius sphere))
         (rsq (* r r))
         (max (+ e r)))
    (when (or (> (v3:x c) max) (> (v3:y c) max) (> (v3:z c) max))
      (return-from query :disjoint))
    (when (or (< (v3:x c) e) (< (v3:y c) e) (< (v3:z c) e))
      (if (and (>= e r) (v3:<= c (v3:vec (- e r))))
          (return-from query :inside)
          (when (and (v3:<= c (v3:vec e))
                     (<= (p3:distance-squared c (v3:vec (- e))) rsq))
            (return-from query :contains)))
      (return-from query :overlaps))
    (if (> (p3:distance-squared c (v3:vec e)) rsq)
        :disjoint
        :overlaps)))

(u:fn-> search (octree p3:point u:f32) list)
(defun search (octree point radius)
  (declare (optimize speed))
  (let ((sphere (sphere:sphere :origin point :radius radius))
        (radius-squared (expt radius 2))
        (points (points octree))
        (points-maybe nil)
        (to-visit (list 0)))
    (u:while t
      (let ((todo nil))
        (dolist (parent-id to-visit)
          (let ((octant (find-octant octree parent-id)))
            (case (query octant sphere)
              ((:overlaps :inside)
               (let ((children (children octant)))
                 (if (zerop (da:length children))
                     (da:map children (lambda (x) (push x todo)))
                     (da:map (point-indices octant)
                             (lambda (x) (push (aref points x) points-maybe))))))
              (:contains
               (da:map (point-indices octant)
                       (lambda (x) (push (aref points x) points-maybe)))))))
        (unless todo
          (loop-finish))
        (setf to-visit todo)))
    (remove-if-not
     (lambda (x)
       (< (p3:distance-squared x (sphere:origin sphere)) radius-squared))
     points-maybe)))
