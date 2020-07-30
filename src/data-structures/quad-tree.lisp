(in-package #:cl-user)

;;;; An implementation of the quad tree data structure.

(defpackage #:net.mfiano.lisp.algae.data-structures.quad-tree
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v2 #:net.mfiano.lisp.origin.vec2))
  (:use #:cl)
  (:export
   #:boundary
   #:insert
   #:make-boundary
   #:make-tree
   #:query
   #:tree))

(in-package #:net.mfiano.lisp.algae.data-structures.quad-tree)

(defstruct (boundary
            (:constructor make-boundary (center extents))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (center (v2:vec) :type v2:vec)
  (extents (v2:vec) :type v2:vec))

(defstruct (tree
            (:constructor %%make-tree)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  boundary
  (max-capacity 0 :type fixnum)
  (max-depth 0 :type fixnum)
  (depth 0 :type fixnum)
  nw
  ne
  sw
  se
  points)

(u:define-printer (tree stream :type nil :identity t)
  (format stream "QUAD-TREE"))

(u:defun-inline %make-tree (boundary depth max-depth max-capacity)
  (declare (optimize speed))
  (%%make-tree :boundary boundary
               :depth depth
               :max-depth max-depth
               :max-capacity max-capacity))

(defun make-tree (boundary &key (max-depth 3) (max-capacity 8))
  (%make-tree boundary 0 max-depth max-capacity))

(defun contains-point-p (boundary point)
  (declare (optimize speed)
           (v2:vec point))
  (v2:with-components ((p point)
                       (c (center boundary))
                       (e (extents boundary)))
    (and (>= (+ cx ex) px (- cx ex))
         (>= (+ cy ey) py (- cy ey)))))

(defun intersects-p (boundary-a boundary-b)
  (declare (optimize speed))
  (v2:with-components ((ac (center boundary-a))
                       (ae (extents boundary-a))
                       (bc (center boundary-b))
                       (be (extents boundary-b)))
    (and (>= (+ bcx bex) (- acx aex))
         (>= (+ bcy bey) (- acy aey))
         (<= (- bcx bex) (+ acx aex))
         (<= (- bcy bey) (+ acy aey)))))

(u:defun-inline sub-divided-p (tree)
  (declare (optimize speed))
  (when (nw tree)
    t))

(u:defun-inline %insert (tree point)
  (or (insert (nw tree) point)
      (insert (ne tree) point)
      (insert (sw tree) point)
      (insert (se tree) point)))

(defun sub-divide (tree)
  (declare (optimize speed))
  (flet ((make-quadrant (tree boundary)
           (%make-tree boundary
                       (1+ (depth tree))
                       (max-depth tree)
                       (max-capacity tree))))
    (let ((boundary (boundary tree)))
      (v2:with-components ((c (center boundary))
                           (e (v2:scale (extents boundary) 0.5)))
        (let ((x1 (- cx ex))
              (x2 (+ cx ex))
              (y1 (+ cy ey))
              (y2 (- cy ey)))
          (setf (nw tree) (make-quadrant tree (make-boundary (v2:vec x1 y1) e))
                (ne tree) (make-quadrant tree (make-boundary (v2:vec x2 y1) e))
                (sw tree) (make-quadrant tree (make-boundary (v2:vec x1 y2) e))
                (se tree) (make-quadrant tree (make-boundary (v2:vec x2 y2) e)))
          (loop :for point = (pop (points tree))
                :while point
                :do (%insert tree point)))))))

(defun insert (tree point)
  (declare (optimize speed))
  (cond
    ((not (contains-point-p (boundary tree) point))
     nil)
    ((or (< (list-length (points tree)) (max-capacity tree))
         (<= (max-depth tree) (depth tree)))
     (push point (points tree)))
    (t
     (unless (sub-divided-p tree)
       (sub-divide tree))
     (%insert tree point))))

(defun query (tree boundary)
  (declare (optimize speed))
  (let (points)
    (labels ((%query (tree)
               (when (intersects-p (boundary tree) boundary)
                 (dolist (point (points tree))
                   (when (contains-point-p boundary point)
                     (push point points)))
                 (when (sub-divided-p tree)
                   (%query (nw tree))
                   (%query (ne tree))
                   (%query (sw tree))
                   (%query (se tree))))))
      (%query tree)
      points)))
