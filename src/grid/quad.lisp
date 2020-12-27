(in-package #:algae.grid)

(defclass quad-grid (grid) ())

(defmethod initialize-instance :after ((instance quad-grid) &key)
  (with-slots (%edge-directions %corner-directions) instance
    (setf %edge-directions '(:e :n :w :s)
          %corner-directions '(:ne :nw :sw :se))))

(defmethod nudge ((grid quad-grid) cell)
  (v2:+ cell (v2:vec 1e-7)))

(defmethod neighbor-by-index ((grid quad-grid) cell index)
  (v2:+ cell (aref (neighbor-offsets grid) index)))

(defmethod to-point ((grid quad-grid) cell)
  (with-slots (%cell-size %cell-origin) grid
    (v2:with-components ((c cell)
                         (s %cell-size))
      (let ((px (* cx sx))
            (py (* cy sy)))
        (v2:round (v2:+ (v2:vec px py) %cell-origin))))))

(defmethod from-point ((grid quad-grid) point)
  (with-slots (%cell-size %cell-origin) grid
    (v2:with-components ((p (v2:- point %cell-origin))
                         (s %cell-size))
      (let ((px (/ px sx))
            (py (/ py sy)))
        (v2:round (v2:vec px py))))))

(defmethod select-line ((grid quad-grid) cell1 cell2)
  (loop :with distance = (distance grid cell1 cell2)
        :with step = (/ (max distance 1))
        :with start = (nudge grid cell1)
        :with end = (nudge grid cell2)
        :for cell :to distance
        :for selected = (v2:round (v2:lerp start end (float (* cell step) 1f0)))
        :when (cell-p grid selected)
          :collect selected))

(defmethod select-range ((grid quad-grid) cell range)
  (loop :with cells
        :for x :from (- range) :to range
        :do (loop :for y :from (- range) :to range
                  :for selected = (v2:+ cell (v2:vec x y))
                  :when (cell-p grid selected)
                    :do (push selected cells))
        :finally (return (nreverse cells))))
