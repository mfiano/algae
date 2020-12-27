(in-package #:algae.grid)

(defclass hex-grid/columns (hex-grid) ())

(defmethod initialize-instance :after ((instance hex-grid/columns) &key)
  (with-slots (%forward %inverse %edge-directions %corner-directions) instance
    (setf %forward (v4:vec (/ 3 2) 0 (/ (sqrt 3) 2) (sqrt 3))
          %inverse (v4:vec (/ 2 3) 0 (/ -1 3) (/ (sqrt 3) 3))
          %edge-directions '(:ne :n :nw :sw :s :se)
          %corner-directions '(:e :ne :nw :w :sw :se))))

(defmethod neighbor-directions ((grid hex-grid/columns))
  '(:se :ne :n :nw :sw :s))

(defmethod to-cell ((grid hex-grid/columns) hex)
  (v3:with-components ((h hex))
    (let ((y (+ hy (/ (+ hx (* (hex-offset grid) (mod hx 2))) 2))))
      (v2:vec hx y))))

(defmethod from-cell ((grid hex-grid/columns) cell)
  (v2:with-components ((c cell))
    (let ((y (- cy (/ (+ cx (* (hex-offset grid) (mod cx 2))) 2))))
      (make-hex cx y))))
