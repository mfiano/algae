(in-package #:net.mfiano.lisp.algae.noise)

(u:define-constant +open-simplex-2d/stretch+ (/ (1- (/ (sqrt 3d0))) 2))

(u:define-constant +open-simplex-2d/squish+ (/ (1- (sqrt 3d0)) 2))

(u:define-constant +open-simplex-2d/scale+ (/ 47d0))

(u:define-constant +open-simplex-2d/gradients+
    (let ((data '(5 2 2 5 -5 2 -2 5 5 -2 2 -5 -5 -2 -2 -5)))
      (make-array 16 :element-type 'fixnum :initial-contents data))
  :test #'equalp)

(declaim (inline %make-open-simplex-2d-state))
(defstruct (open-simplex-2d-state
            (:constructor %make-open-simplex-2d-state)
            (:conc-name oss2-)
            (:predicate nil)
            (:copier nil))
  (stretch-offset 0d0 :type double-float)
  (xsb 0 :type fixnum)
  (ysb 0 :type fixnum)
  (dx0 0d0 :type double-float)
  (dy0 0d0 :type double-float)
  (dx1 0d0 :type double-float)
  (dy1 0d0 :type double-float)
  (dx2 0d0 :type double-float)
  (dy2 0d0 :type double-float)
  (dx-ext 0d0 :type double-float)
  (dy-ext 0d0 :type double-float)
  (xsv-ext 0 :type fixnum)
  (ysv-ext 0 :type fixnum)
  (xins 0d0 :type double-float)
  (yins 0d0 :type double-float)
  (ins 0d0 :type double-float)
  (value 0d0 :type double-float))

(u:defun-inline make-open-simplex-2d-state (x y)
  (let* ((stretch-offset (* (+ x y) +open-simplex-2d/stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (squish-offset (* (+ xsb ysb) +open-simplex-2d/squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb)))
    (declare (f50 xs ys))
    (%make-open-simplex-2d-state
     :xsb xsb
     :ysb ysb
     :dx0 dx0
     :dy0 dy0
     :dx1 (- dx0 1 +open-simplex-2d/squish+)
     :dy1 (- dy0 +open-simplex-2d/squish+)
     :dx2 (- dx0 +open-simplex-2d/squish+)
     :dy2 (- dy0 1 +open-simplex-2d/squish+)
     :xins xins
     :yins yins
     :ins (+ xins yins))))

(u:defun-inline open-simplex-2d/extrapolate (xsb ysb dx dy)
  (let ((index (logand (pget +permutation+ ysb xsb) 14)))
    (+ (* (aref +open-simplex-2d/gradients+ index) dx)
       (* (aref +open-simplex-2d/gradients+ (1+ index)) dy))))

(u:defun-inline open-simplex-2d/contribute (state dx dy xsb ysb)
  (let ((a (- 2 (* dx dx) (* dy dy))))
    (when (plusp a)
      (incf (oss2-value state)
            (* (expt a 4) (open-simplex-2d/extrapolate xsb ysb dx dy))))
    (values)))

(u:defun-inline open-simplex-2d/contribute1 (state)
  (let ((xsb (oss2-xsb state))
        (ysb (oss2-ysb state)))
    (open-simplex-2d/contribute state
                                (oss2-dx1 state)
                                (oss2-dy1 state)
                                (1+ xsb)
                                ysb)
    (open-simplex-2d/contribute state
                                (oss2-dx2 state)
                                (oss2-dy2 state)
                                xsb
                                (1+ ysb))))

(u:defun-inline open-simplex-2d/contribute2 (state)
  (open-simplex-2d/contribute state
                              (oss2-dx0 state)
                              (oss2-dy0 state)
                              (oss2-xsb state)
                              (oss2-ysb state))
  (open-simplex-2d/contribute state
                              (oss2-dx-ext state)
                              (oss2-dy-ext state)
                              (oss2-xsv-ext state)
                              (oss2-ysv-ext state)))

(u:defun-inline open-simplex-2d/in1 (state)
  (let ((xins (oss2-xins state))
        (yins (oss2-yins state))
        (zins (- 1 (oss2-ins state)))
        (xsb (oss2-xsb state))
        (ysb (oss2-ysb state))
        (dx0 (oss2-dx0 state))
        (dy0 (oss2-dy0 state))
        (sq2 #.(* +open-simplex-2d/squish+ 2)))
    (if (or (> zins xins) (> zins yins))
        (if (> xins yins)
            (psetf (oss2-xsv-ext state) (1+ xsb)
                   (oss2-ysv-ext state) (1- ysb)
                   (oss2-dx-ext state) (1- dx0)
                   (oss2-dy-ext state) (1+ dy0))
            (psetf (oss2-xsv-ext state) (1- xsb)
                   (oss2-ysv-ext state) (1+ ysb)
                   (oss2-dx-ext state) (1+ dx0)
                   (oss2-dy-ext state) (1- dy0)))
        (psetf (oss2-xsv-ext state) (1+ xsb)
               (oss2-ysv-ext state) (1+ ysb)
               (oss2-dx-ext state) (- dx0 1 sq2)
               (oss2-dy-ext state) (- dy0 1 sq2)))
    (values)))

(u:defun-inline open-simplex-2d/in2 (state)
  (let ((xins (oss2-xins state))
        (yins (oss2-yins state))
        (zins (- 2 (oss2-ins state)))
        (xsb (oss2-xsb state))
        (ysb (oss2-ysb state))
        (dx0 (oss2-dx0 state))
        (dy0 (oss2-dy0 state))
        (sq2 #.(* +open-simplex-2d/squish+ 2)))
    (if (or (< zins xins) (< zins yins))
        (if (> xins yins)
            (psetf (oss2-xsv-ext state) (+ xsb 2)
                   (oss2-ysv-ext state) ysb
                   (oss2-dx-ext state) (- dx0 2 sq2)
                   (oss2-dy-ext state) (- dy0 sq2))
            (psetf (oss2-xsv-ext state) xsb
                   (oss2-ysv-ext state) (+ ysb 2)
                   (oss2-dx-ext state) (- dx0 sq2)
                   (oss2-dy-ext state) (- dy0 2 sq2)))
        (psetf (oss2-dx-ext state) dx0
               (oss2-dy-ext state) dy0
               (oss2-xsv-ext state) xsb
               (oss2-ysv-ext state) ysb))
    (incf (oss2-xsb state))
    (incf (oss2-ysb state))
    (psetf (oss2-dx0 state) (- dx0 1 sq2)
           (oss2-dy0 state) (- dy0 1 sq2))
    (values)))

(u:defun-inline %open-simplex-2d (x y)
  (declare (optimize speed)
           (double-float x y))
  (let ((state (make-open-simplex-2d-state x y)))
    (open-simplex-2d/contribute1 state)
    (if (<= (oss2-ins state) 1)
        (open-simplex-2d/in1 state)
        (open-simplex-2d/in2 state))
    (open-simplex-2d/contribute2 state)
    (float (* (oss2-value state) +open-simplex-2d/scale+) 1f0)))

(defun open-simplex-2d (x y)
  (declare (real x y))
  (%open-simplex-2d (float x 1d0) (float y 1d0)))
