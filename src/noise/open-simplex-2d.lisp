(in-package #:cl-user)

(defpackage #:algae.noise.open-simplex-2d
  (:local-nicknames
   (#:c #:algae.noise.common)
   (#:rng #:algae.rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:algae.noise.open-simplex-2d)

(u:define-constant +stretch+ (/ (1- (/ (sqrt 3d0))) 2))

(u:define-constant +squish+ (/ (1- (sqrt 3d0)) 2))

(u:define-constant +scale+ (/ 47d0))

(u:define-constant +gradients+
    (let ((data '(5 2 2 5 -5 2 -2 5 5 -2 2 -5 -5 -2 -2 -5)))
      (make-array 16 :element-type 'fixnum :initial-contents data))
  :test #'equalp)

(declaim (inline %make-state))
(defstruct (state
            (:constructor %make-state)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (table c:+perlin-permutation+ :type (simple-array u:ub8 (512)))
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

(u:defun-inline make-state (table x y)
  (let* ((stretch-offset (* (+ x y) +stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (squish-offset (* (+ xsb ysb) +squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb)))
    (declare (c:f50 xs ys))
    (%make-state :table table
                 :xsb xsb
                 :ysb ysb
                 :dx0 dx0
                 :dy0 dy0
                 :dx1 (- dx0 1 +squish+)
                 :dy1 (- dy0 +squish+)
                 :dx2 (- dx0 +squish+)
                 :dy2 (- dy0 1 +squish+)
                 :xins xins
                 :yins yins
                 :ins (+ xins yins))))

(u:defun-inline extrapolate (table xsb ysb dx dy)
  (let ((index (logand (c:pget table ysb xsb) 14)))
    (+ (* (aref +gradients+ index) dx)
       (* (aref +gradients+ (1+ index)) dy))))

(u:defun-inline contribute (state dx dy xsb ysb)
  (let ((a (- 2 (* dx dx) (* dy dy))))
    (when (plusp a)
      (incf (value state)
            (* (expt a 4) (extrapolate (table state) xsb ysb dx dy))))
    (values)))

(u:defun-inline contribute1 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state)))
    (contribute state (dx1 state) (dy1 state) (1+ xsb) ysb)
    (contribute state (dx2 state) (dy2 state) xsb (1+ ysb))))

(u:defun-inline contribute2 (state)
  (contribute state (dx0 state) (dy0 state) (xsb state) (ysb state))
  (contribute state (dx-ext state) (dy-ext state) (xsv-ext state)
              (ysv-ext state)))

(u:defun-inline in1 (state)
  (let ((xins (xins state))
        (yins (yins state))
        (zins (- 1 (ins state)))
        (xsb (xsb state))
        (ysb (ysb state))
        (dx0 (dx0 state))
        (dy0 (dy0 state))
        (sq2 #.(* +squish+ 2)))
    (if (or (> zins xins) (> zins yins))
        (if (> xins yins)
            (psetf (xsv-ext state) (1+ xsb)
                   (ysv-ext state) (1- ysb)
                   (dx-ext state) (1- dx0)
                   (dy-ext state) (1+ dy0))
            (psetf (xsv-ext state) (1- xsb)
                   (ysv-ext state) (1+ ysb)
                   (dx-ext state) (1+ dx0)
                   (dy-ext state) (1- dy0)))
        (psetf (xsv-ext state) (1+ xsb)
               (ysv-ext state) (1+ ysb)
               (dx-ext state) (- dx0 1 sq2)
               (dy-ext state) (- dy0 1 sq2)))
    (values)))

(u:defun-inline in2 (state)
  (let ((xins (xins state))
        (yins (yins state))
        (zins (- 2 (ins state)))
        (xsb (xsb state))
        (ysb (ysb state))
        (dx0 (dx0 state))
        (dy0 (dy0 state))
        (sq2 #.(* +squish+ 2)))
    (if (or (< zins xins) (< zins yins))
        (if (> xins yins)
            (psetf (xsv-ext state) (+ xsb 2)
                   (ysv-ext state) ysb
                   (dx-ext state) (- dx0 2 sq2)
                   (dy-ext state) (- dy0 sq2))
            (psetf (xsv-ext state) xsb
                   (ysv-ext state) (+ ysb 2)
                   (dx-ext state) (- dx0 sq2)
                   (dy-ext state) (- dy0 2 sq2)))
        (psetf (dx-ext state) dx0
               (dy-ext state) dy0
               (xsv-ext state) xsb
               (ysv-ext state) ysb))
    (incf (xsb state))
    (incf (ysb state))
    (psetf (dx0 state) (- dx0 1 sq2)
           (dy0 state) (- dy0 1 sq2))
    (values)))

(u:defun-inline sample (table x y)
  (declare (optimize speed)
           (double-float x y))
  (let ((state (make-state table x y)))
    (contribute1 state)
    (if (<= (ins state) 1)
        (in1 state)
        (in2 state))
    (contribute2 state)
    (float (* (value state) +scale+) 1f0)))

(defmethod c::%make-sampler-func ((type (eql :open-simplex-2d)))
  (let ((table (rng:shuffle 'c::rng c:+perlin-permutation+)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (sample table x y))))
