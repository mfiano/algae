(in-package #:net.mfiano.lisp.algae.grid)

(defclass hex-grid (grid)
  ((%forward :reader forward)
   (%inverse :reader inverse)
   (%offset :reader offset
            :initarg :offset
            :initform :even)))

(defgeneric to-cell (grid hex))

(defgeneric from-cell (grid cell))

(defun hex-offset (grid)
  (ecase (offset grid)
    (:even 1)
    (:odd -1)))

(defun make-hex (x y)
  (v3:vec x y (- (- x) y)))

(defmethod nudge ((grid hex-grid) hex)
  (v3:+ hex (make-hex 1e-7 1e-7)))

(defun hex-round (hex)
  (v3:with-components ((r (v3:round hex))
                       (d (v3:abs (v3:- r hex))))
    (cond ((and (> dx dy) (> dx dz))
           (setf rx (- (- ry) rz)))
          ((> dy dz)
           (setf ry (- (- rx) rz))))
    r))

(defmethod neighbor-offsets ((grid hex-grid))
  (vector (make-hex 1 0)
          (make-hex 1 -1)
          (make-hex 0 -1)
          (make-hex -1 0)
          (make-hex -1 1)
          (make-hex 0 1)))

(defmethod neighbor-by-index ((grid hex-grid) cell index)
  (let ((hex (v3:+ (from-cell grid cell)
                   (aref (neighbor-offsets grid) index))))
    (to-cell grid hex)))

(defmethod distance ((grid hex-grid) cell1 cell2)
  (v3:with-components ((c (v3:abs (from-cell grid (v2:- cell1 cell2)))))
    (floor (max cx cy cz))))

(defmethod to-point ((grid hex-grid) cell)
  (with-slots (%cell-size %cell-origin %forward) grid
    (v3:with-components ((c (from-cell grid cell))
                         (s %cell-size))
      (v4:with-components ((f %forward))
        (let* ((x (+ (* fw cx) (* fx cy)))
               (y (+ (* fy cx) (* fz cy)))
               (px (* x sx))
               (py (* y sy)))
          (v2:round (v2:+ (v2:vec px py) %cell-origin)))))))

(defmethod from-point ((grid hex-grid) point)
  (with-slots (%cell-size %cell-origin %inverse) grid
    (v2:with-components ((p (v2:/ (v2:- point %cell-origin) %cell-size)))
      (v4:with-components ((i %inverse))
        (let* ((x (+ (* iw px) (* ix py)))
               (y (+ (* iy px) (* iz py))))
          (to-cell grid (hex-round (make-hex x y))))))))

(defmethod select-line ((grid hex-grid) cell1 cell2)
  (loop :with distance = (distance grid cell1 cell2)
        :with step = (/ (max distance 1))
        :with start = (nudge grid (from-cell grid cell1))
        :with end = (nudge grid (from-cell grid cell2))
        :for hex :to distance
        :for selected = (v3:lerp start end (float (* hex step) 1f0))
        :for cell = (to-cell grid (hex-round selected))
        :when (cell-p grid cell)
          :collect cell))

(defmethod select-range ((grid hex-grid) cell range)
  (loop :with cells
        :for x :from (- range) :to range
        :for min = (max (- range) (- (- x) range))
        :for max = (min range (+ (- x) range))
        :do (loop :for y :from min :to max
                  :for hex = (v3:+ (from-cell grid cell) (v3:vec x y))
                  :for selected = (to-cell grid hex)
                  :when (cell-p grid selected)
                    :do (push selected cells))
        :finally (return (nreverse cells))))
