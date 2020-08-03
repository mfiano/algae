(in-package #:net.mfiano.lisp.algae.grid)

(defclass grid ()
  ((%size :reader size
          :initarg :size)
   (%cell-size :reader cell-size
               :initarg :cell-size
               :initform nil)
   (%cell-origin :reader cell-origin
                 :initarg :cell-origin
                 :initform nil)
   (%edge-directions :reader edge-directions)
   (%corner-directions :reader corner-directions)))

(defmethod initialize-instance :after ((instance grid) &key size)
  (unless size
    (error "Grid must have a size."))
  (with-slots (%cell-size %cell-origin) instance
    (setf %cell-size (or %cell-size (v2:vec 1))
          %cell-origin (or %cell-origin (v2:vec)))))

(defun make-grid (type &rest args)
  (apply #'make-instance type args))

(defun cell-p (grid cell)
  (v2:with-components ((g (size grid))
                       (c cell))
    (and (>= cx 0)
         (< cx gx)
         (>= cy 0)
         (< cy gy))))

(defun check-cell (grid cell)
  (unless (cell-p grid cell)
    (error "Cell ~s is not a member of the grid." cell)))

(defgeneric nudge (grid cell))

(defgeneric neighbor-directions (grid))

(defgeneric neighbor-offsets (grid))

(defgeneric neighbor-by-index (grid cell index)
  (:method :before (grid cell index)
    (check-cell grid cell)))

(defgeneric distance (grid cell1 cell2)
  (:method :before (grid cell1 cell2)
    (check-cell grid cell1)
    (check-cell grid cell2)))

(defgeneric to-point (grid cell)
  (:method :before (grid cell)
    (check-cell grid cell)))

(defgeneric from-point (grid point))

(defgeneric select-line (grid cell1 cell2)
  (:method :before (grid cell1 cell2)
    (check-cell grid cell1)
    (check-cell grid cell2)))

(defgeneric select-range (grid cell range)
  (:method :before (grid cell range)
    (check-cell grid cell)))

(defun neighbors (grid cell)
  (loop :for direction :in (neighbor-directions grid)
        :for i :from 0
        :for neighbor = (neighbor-by-index grid cell i)
        :when (cell-p grid neighbor)
          :collect direction
          :and
            :collect neighbor))

(defun neighbors-p (grid cell1 cell2)
  (let ((neighbors (u:plist-values (neighbors grid cell1))))
    (when (find cell2 neighbors :test #'equalp)
      t)))

(defun neighbor (grid cell direction)
  (getf (neighbors grid cell) direction))
