(in-package #:cl-user)

(defpackage #:algae.noise.open-simplex-3d
  (:local-nicknames
   (#:c #:algae.noise.common)
   (#:rng #:algae.rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:algae.noise.open-simplex-3d)

(u:define-constant +stretch+ (/ -6d0))

(u:define-constant +squish+ (/ 3d0))

(u:define-constant +scale+ (/ 103d0))

(u:define-constant +permutation+
    (let ((data '(21 48 51 57 54 45 33 39 27 69 0 15 6 51 21 27 60 36 21 18 63 66 24 9 39 0 63 30 69
                  66 18 12 21 0 54 9 0 6 15 42 66 36 9 33 63 33 33 24 27 27 27 48 63 15 24 45 18 60
                  15 48 9 0 60 21 6 63 69 42 57 0 9 66 15 6 42 45 33 45 39 6 36 57 39 42 12 27 60 51
                  21 66 15 48 12 18 69 18 51 3 45 51 3 0 24 3 51 12 36 57 48 51 54 3 24 12 45 30 60
                  60 45 42 60 12 39 18 15 54 9 48 12 3 30 30 12 9 15 30 42 9 66 18 45 30 39 60 45 42
                  33 33 69 48 30 51 42 63 12 54 21 45 6 63 69 24 24 6 60 30 57 66 15 27 15 33 69 57
                  12 27 27 66 45 39 57 6 36 42 21 51 24 48 30 51 48 24 6 18 3 36 33 30 6 3 66 54 0 36
                  69 33 54 3 27 9 3 57 27 42 69 33 3 0 66 21 39 21 30 39 48 36 36 24 57 3 6 63 21 12
                  18 42 54 60 39 63 18 54 57 15 0 0 9 63 24 9 18 54 69 39 36 36)))
      (make-array 256 :element-type 'u:ub8 :initial-contents data))
  :test #'equalp)

(u:define-constant +gradients+
    (let ((data '(-11 4 4 -4 11 4 -4 4 11 11 4 4 4 11 4 4 4 11 -11 -4 4 -4 -11 4 -4 -4 11 11 -4 4 4
                  -11 4 4 -4 11 -11 4 -4 -4 11 -4 -4 4 -11 11 4 -4 4 11 -4 4 4 -11 -11 -4 -4 -4 -11
                  -4 -4 -4 -11 11 -4 -4 4 11 -4 4 -4 -11)))
      (make-array 72 :element-type 'fixnum :initial-contents data))
  :test #'equalp)

(declaim (inline %make-state))
(defstruct (state
            (:constructor %make-state)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (table +permutation+ :type (simple-array u:ub8 (256)))
  (stretch-offset 0d0 :type double-float)
  (xsb 0 :type fixnum)
  (ysb 0 :type fixnum)
  (zsb 0 :type fixnum)
  (dx0 0d0 :type double-float)
  (dy0 0d0 :type double-float)
  (dz0 0d0 :type double-float)
  (dx1 0d0 :type double-float)
  (dy1 0d0 :type double-float)
  (dz1 0d0 :type double-float)
  (dx2 0d0 :type double-float)
  (dy2 0d0 :type double-float)
  (dz2 0d0 :type double-float)
  (dx3 0d0 :type double-float)
  (dy3 0d0 :type double-float)
  (dz3 0d0 :type double-float)
  (dx4 0d0 :type double-float)
  (dy4 0d0 :type double-float)
  (dz4 0d0 :type double-float)
  (dx5 0d0 :type double-float)
  (dy5 0d0 :type double-float)
  (dz5 0d0 :type double-float)
  (dx6 0d0 :type double-float)
  (dy6 0d0 :type double-float)
  (dz6 0d0 :type double-float)
  (dx-ext0 0d0 :type double-float)
  (dy-ext0 0d0 :type double-float)
  (dz-ext0 0d0 :type double-float)
  (dx-ext1 0d0 :type double-float)
  (dy-ext1 0d0 :type double-float)
  (dz-ext1 0d0 :type double-float)
  (xsv-ext0 0 :type fixnum)
  (ysv-ext0 0 :type fixnum)
  (zsv-ext0 0 :type fixnum)
  (xsv-ext1 0 :type fixnum)
  (ysv-ext1 0 :type fixnum)
  (zsv-ext1 0 :type fixnum)
  (xins 0d0 :type double-float)
  (yins 0d0 :type double-float)
  (zins 0d0 :type double-float)
  (ins 0d0 :type double-float)
  (value 0d0 :type double-float))

(u:defun-inline make-state (table x y z)
  (let* ((stretch-offset (* (+ x y z) +stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (zs (+ z stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (zsb (floor zs))
         (squish-offset (* (+ xsb ysb zsb) +squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (dz0 (- z (+ zsb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb))
         (zins (- zs zsb)))
    (declare (c:f50 xs ys zs))
    (%make-state :table table
                 :xsb xsb
                 :ysb ysb
                 :zsb zsb
                 :dx0 dx0
                 :dy0 dy0
                 :dz0 dz0
                 :xins xins
                 :yins yins
                 :zins zins
                 :ins (+ xins yins zins))))

(u:defun-inline extrapolate (table xsb ysb zsb dx dy dz)
  (let ((index (c:pget table zsb ysb xsb)))
    (+ (* (aref +gradients+ index) dx)
       (* (aref +gradients+ (1+ index)) dy)
       (* (aref +gradients+ (+ index 2)) dz))))

(u:defun-inline contribute (state dx dy dz xsb ysb zsb)
  (let ((a (- 2 (* dx dx) (* dy dy) (* dz dz))))
    (when (plusp a)
      (incf (value state)
            (* (expt a 4)
               (extrapolate (table state) xsb ysb zsb dx dy dz))))
    (values)))

(defun contribute1 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state)))
    (contribute state (dx0 state) (dy0 state) (dz0 state) xsb ysb zsb)
    (contribute state (dx1 state) (dy1 state) (dz1 state) (1+ xsb) ysb zsb)
    (contribute state (dx2 state) (dy2 state) (dz2 state) xsb (1+ ysb) zsb)
    (contribute state (dx3 state) (dy3 state) (dz3 state) xsb ysb (1+ zsb))))

(defun contribute2 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state)))
    (contribute state (dx3 state) (dy3 state) (dz3 state) (1+ xsb) (1+ ysb) zsb)
    (contribute state (dx2 state) (dy2 state) (dz2 state) (1+ xsb) ysb (1+ zsb))
    (contribute state (dx1 state) (dy1 state) (dz1 state) xsb (1+ ysb) (1+ zsb))
    (contribute state (dx0 state) (dy0 state) (dz0 state) (1+ xsb) (1+ ysb) (1+ zsb))))

(defun contribute3 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state)))
    (contribute state (dx1 state) (dy1 state) (dz1 state) (1+ xsb) ysb zsb)
    (contribute state (dx2 state) (dy2 state) (dz2 state) xsb (1+ ysb) zsb)
    (contribute state (dx3 state) (dy3 state) (dz3 state) xsb ysb (1+ zsb))
    (contribute state (dx4 state) (dy4 state) (dz4 state) (1+ xsb) (1+ ysb) zsb)
    (contribute state (dx5 state) (dy5 state) (dz5 state) (1+ xsb) ysb (1+ zsb))
    (contribute state (dx6 state) (dy6 state) (dz6 state) xsb (1+ ysb) (1+ zsb))))

(defun contribute4 (state)
  (contribute state (dx-ext0 state) (dy-ext0 state) (dz-ext0 state) (xsv-ext0 state) (ysv-ext0 state)
              (zsv-ext0 state))
  (contribute state (dx-ext1 state) (dy-ext1 state) (dz-ext1 state) (xsv-ext1 state) (ysv-ext1 state)
              (zsv-ext1 state)))

(defun in1 (state)
  (let* ((point-a 1)
         (point-b 2)
         (score-a (xins state))
         (score-b (yins state))
         (zins (zins state))
         (wins (- 1 (ins state)))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (sq +squish+)
         (sq2 #.(* +squish+ 2)))
    (cond
      ((and (>= score-a score-b)
            (> zins score-b))
       (psetf score-b zins
              point-b 4))
      ((and (< score-a score-b)
            (> zins score-a))
       (psetf score-a zins
              point-a 4)))
    (if (or (> wins score-a)
            (> wins score-b))
        (let ((c (if (> score-b score-a) point-b point-a)))
          (if (zerop (logand c 1))
              (psetf (xsv-ext0 state) (1- xsb)
                     (xsv-ext1 state) xsb
                     (dx-ext0 state) (1+ dx0)
                     (dx-ext1 state) dx0)
              (let ((xsv (1+ xsb))
                    (dx (1- dx0)))
                (psetf (xsv-ext0 state) xsv
                       (xsv-ext1 state) xsv
                       (dx-ext0 state) dx
                       (dx-ext1 state) dx)))
          (cond
            ((zerop (logand c 2))
             (psetf (ysv-ext0 state) ysb
                    (ysv-ext1 state) ysb
                    (dy-ext0 state) dy0
                    (dy-ext1 state) dy0)
             (cond
               ((zerop (logand c 1))
                (decf (ysv-ext1 state))
                (incf (dy-ext1 state)))
               (t
                (decf (ysv-ext0 state))
                (incf (dy-ext0 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (1- dy0)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))))
          (if (zerop (logand c 4))
              (psetf (zsv-ext0 state) zsb
                     (zsv-ext1 state) (1- zsb)
                     (dz-ext0 state) dz0
                     (dz-ext1 state) (1+ dz0))
              (let ((zsv (1+ zsb))
                    (dz (1- dz0)))
                (psetf (zsv-ext0 state) zsv
                       (zsv-ext1 state) zsv
                       (dz-ext0 state) dz
                       (dz-ext1 state) dz))))
        (let ((c (logand (logior point-a point-b) 255)))
          (if (zerop (logand c 1))
              (psetf (xsv-ext0 state) xsb
                     (xsv-ext1 state) (1- xsb)
                     (dx-ext0 state) (- dx0 sq2)
                     (dx-ext1 state) (- (1+ dx0) sq))
              (let ((xsv (1+ xsb)))
                (psetf (xsv-ext0 state) xsv
                       (xsv-ext1 state) xsv
                       (dx-ext0 state) (- dx0 1 sq2)
                       (dx-ext1 state) (- dx0 1 sq))))
          (if (zerop (logand c 2))
              (psetf (ysv-ext0 state) ysb
                     (ysv-ext1 state) (1- ysb)
                     (dy-ext0 state) (- dy0 sq2)
                     (dy-ext1 state) (- (1+ dy0) sq))
              (let ((ysv (1+ ysb)))
                (psetf (ysv-ext0 state) ysv
                       (ysv-ext1 state) ysv
                       (dy-ext0 state) (- dy0 1 sq2)
                       (dy-ext1 state) (- dy0 1 sq))))
          (if (zerop (logand c 4))
              (psetf (zsv-ext0 state) zsb
                     (zsv-ext1 state) (1- zsb)
                     (dz-ext0 state) (- dz0 sq2)
                     (dz-ext1 state) (- (1+ dz0) sq))
              (let ((zsv (1+ zsb)))
                (psetf (zsv-ext0 state) zsv
                       (zsv-ext1 state) zsv
                       (dz-ext0 state) (- dz0 1 sq2)
                       (dz-ext1 state) (- dz0 1 sq))))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dx2 (- dx0 sq)))
      (psetf (dx1 state) (- dx0 1 sq)
             (dy1 state) dy1
             (dz1 state) dz1
             (dx2 state) dx2
             (dy2 state) (- dy0 1 sq)
             (dz2 state) dz1
             (dx3 state) dx2
             (dy3 state) dy1
             (dz3 state) (- dz0 1 sq)))
    (values)))

(defun in2 (state)
  (let* ((point-a 6)
         (point-b 5)
         (score-a (xins state))
         (score-b (yins state))
         (zins (zins state))
         (wins (- 3 (ins state)))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (sq +squish+)
         (sq2 #.(* +squish+ 2))
         (sq3 #.(* +squish+ 3)))
    (cond
      ((and (<= score-a score-b)
            (< zins score-b))
       (psetf score-b zins
              point-b 3))
      ((and (> score-a score-b)
            (< zins score-a))
       (psetf score-a zins
              point-a 3)))
    (if (or (< wins score-a)
            (< wins score-b))
        (let ((c (if (< score-b score-a) point-b point-a)))
          (if (not (zerop (logand c 1)))
              (psetf (xsv-ext0 state) (+ xsb 2)
                     (xsv-ext1 state) (1+ xsb)
                     (dx-ext0 state) (- dx0 2 sq3)
                     (dx-ext1 state) (- dx0 1 sq3))
              (let ((dx (- dx0 sq3)))
                (psetf (xsv-ext0 state) xsb
                       (xsv-ext1 state) xsb
                       (dx-ext0 state) dx
                       (dx-ext1 state) dx)))
          (cond
            ((not (zerop (logand c 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq3)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))
             (cond
               ((not (zerop (logand c 1)))
                (incf (ysv-ext1 state))
                (decf (dy-ext1 state)))
               (t
                (incf (ysv-ext0 state))
                (decf (dy-ext0 state)))))
            (t
             (let ((dy (- dy0 sq3)))
               (psetf (ysv-ext0 state) ysb
                      (ysv-ext1 state) ysb
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))))
          (if (not (zerop (logand c 4)))
              (psetf (zsv-ext0 state) (1+ zsb)
                     (zsv-ext1 state) (+ zsb 2)
                     (dz-ext0 state) (- dz0 1 sq3)
                     (dz-ext1 state) (- dz0 2 sq3))
              (let ((dz (- dz0 sq3)))
                (psetf (zsv-ext0 state) zsb
                       (zsv-ext1 state) zsb
                       (dz-ext0 state) dz
                       (dz-ext1 state) dz))))
        (let ((c (logand (logand point-a point-b) 255)))
          (if (not (zerop (logand c 1)))
              (psetf (xsv-ext0 state) (1+ xsb)
                     (xsv-ext1 state) (+ xsb 2)
                     (dx-ext0 state) (- dx0 1 sq)
                     (dx-ext1 state) (- dx0 2 sq2))
              (psetf (xsv-ext0 state) xsb
                     (xsv-ext1 state) xsb
                     (dx-ext0 state) (- dx0 sq)
                     (dx-ext1 state) (- dx0 sq2)))
          (if (not (zerop (logand c 2)))
              (psetf (ysv-ext0 state) (1+ ysb)
                     (ysv-ext1 state) (+ ysb 2)
                     (dy-ext0 state) (- dy0 1 sq)
                     (dy-ext1 state) (- dy0 2 sq2))
              (psetf (ysv-ext0 state) ysb
                     (ysv-ext1 state) ysb
                     (dy-ext0 state) (- dy0 sq)
                     (dy-ext1 state) (- dy0 sq2)))
          (if (not (zerop (logand c 4)))
              (psetf (zsv-ext0 state) (1+ zsb)
                     (zsv-ext1 state) (+ zsb 2)
                     (dz-ext0 state) (- dz0 1 sq)
                     (dz-ext1 state) (- dz0 2 sq2))
              (psetf (zsv-ext0 state) zsb
                     (zsv-ext1 state) zsb
                     (dz-ext0 state) (- dz0 sq)
                     (dz-ext1 state) (- dz0 sq2)))))
    (let ((dz2 (- dz0 1 sq2))
          (dx3 (- dx0 1 sq2))
          (dy3 (- dy0 1 sq2)))
      (psetf (dx3 state) dx3
             (dy3 state) dy3
             (dz3 state) (- dz0 sq2)
             (dx2 state) dx3
             (dy2 state) (- dy0 sq2)
             (dz2 state) dz2
             (dx1 state) (- dx0 sq2)
             (dy1 state) dy3
             (dz1 state) dz2
             (dx0 state) (- dx0 1 sq3)
             (dy0 state) (- dy0 1 sq3)
             (dz0 state) (- dz0 1 sq3)))
    (values)))

(defun in3 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-farthest-p nil)
         (b-farthest-p nil)
         (xins (xins state))
         (yins (yins state))
         (zins (zins state))
         (p1 (+ xins yins))
         (p2 (+ xins zins))
         (p3 (+ yins zins))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (sq +squish+)
         (sq2 #.(* +squish+ 2))
         (sq3 #.(* +squish+ 3)))
    (if (> p1 1)
        (psetf score-a (1- p1)
               point-a 3
               a-farthest-p t)
        (psetf score-a (- 1 p1)
               point-a 4
               a-farthest-p nil))
    (if (> p2 1)
        (psetf score-b (1- p2)
               point-b 5
               b-farthest-p t)
        (psetf score-b (- 1 p2)
               point-b 2
               b-farthest-p nil))
    (if (> p3 1)
        (let ((score (1- p3)))
          (cond
            ((and (<= score-a score-b)
                  (< score-a score))
             (psetf score-a score
                    point-a 6
                    a-farthest-p t))
            ((and (> score-a score-b)
                  (< score-b score))
             (psetf score-b score
                    point-b 6
                    b-farthest-p t))))
        (let ((score (- 1 p3)))
          (cond
            ((and (<= score-a score-b)
                  (< score-a score))
             (psetf score-a score
                    point-a 1
                    a-farthest-p nil))
            ((and (> score-a score-b)
                  (< score-b score))
             (psetf score-b score
                    point-b 1
                    b-farthest-p nil)))))
    (if (eq a-farthest-p b-farthest-p)
        (if a-farthest-p
            (let ((c (logand point-a point-b)))
              (psetf (dx-ext0 state) (- dx0 1 sq3)
                     (dy-ext0 state) (- dy0 1 sq3)
                     (dz-ext0 state) (- dz0 1 sq3)
                     (xsv-ext0 state) (1+ xsb)
                     (ysv-ext0 state) (1+ ysb)
                     (zsv-ext0 state) (1+ zsb))
              (cond
                ((not (zerop (logand c 1)))
                 (psetf (dx-ext1 state) (- dx0 2 sq2)
                        (dy-ext1 state) (- dy0 sq2)
                        (dz-ext1 state) (- dz0 sq2)
                        (xsv-ext1 state) (+ xsb 2)
                        (ysv-ext1 state) ysb
                        (zsv-ext1 state) zsb))
                ((not (zerop (logand c 2)))
                 (psetf (dx-ext1 state) (- dx0 sq2)
                        (dy-ext1 state) (- dy0 2 sq2)
                        (dz-ext1 state) (- dz0 sq2)
                        (xsv-ext1 state) xsb
                        (ysv-ext1 state) (+ ysb 2)
                        (zsv-ext1 state) zsb))
                (t
                 (psetf (dx-ext1 state) (- dx0 sq2)
                        (dy-ext1 state) (- dy0 sq2)
                        (dz-ext1 state) (- dz0 2 sq2)
                        (xsv-ext1 state) xsb
                        (ysv-ext1 state) ysb
                        (zsv-ext1 state) (+ zsb 2)))))
            (let ((c (logior point-a point-b)))
              (psetf (dx-ext0 state) dx0
                     (dy-ext0 state) dy0
                     (dz-ext0 state) dz0
                     (xsv-ext0 state) xsb
                     (ysv-ext0 state) ysb
                     (zsv-ext0 state) zsb)
              (cond
                ((zerop (logand c 1))
                 (psetf (dx-ext1 state) (- (1+ dx0) sq)
                        (dy-ext1 state) (- dy0 1 sq)
                        (dz-ext1 state) (- dz0 1 sq)
                        (xsv-ext1 state) (1- xsb)
                        (ysv-ext1 state) (1+ ysb)
                        (zsv-ext1 state) (1+ zsb)))
                ((zerop (logand c 2))
                 (psetf (dx-ext1 state) (- dx0 1 sq)
                        (dy-ext1 state) (- (1+ dy0) sq)
                        (dz-ext1 state) (- dz0 1 sq)
                        (xsv-ext1 state) (1+ xsb)
                        (ysv-ext1 state) (1- ysb)
                        (zsv-ext1 state) (1+ zsb)))
                (t
                 (psetf (dx-ext1 state) (- dx0 1 sq)
                        (dy-ext1 state) (- dy0 1 sq)
                        (dz-ext1 state) (- (1+ dz0) sq)
                        (xsv-ext1 state) (1+ xsb)
                        (ysv-ext1 state) (1+ ysb)
                        (zsv-ext1 state) (1- zsb))))))
        (let ((c1 (if a-farthest-p point-a point-b))
              (c2 (if a-farthest-p point-b point-a)))
          (cond
            ((zerop (logand c1 1))
             (psetf (dx-ext0 state) (- (1+ dx0) sq)
                    (dy-ext0 state) (- dy0 1 sq)
                    (dz-ext0 state) (- dz0 1 sq)
                    (xsv-ext0 state) (1- xsb)
                    (ysv-ext0 state) (1+ ysb)
                    (zsv-ext0 state) (1+ zsb)))
            ((zerop (logand c1 2))
             (psetf (dx-ext0 state) (- dx0 1 sq)
                    (dy-ext0 state) (- (1+ dy0) sq)
                    (dz-ext0 state) (- dz0 1 sq)
                    (xsv-ext0 state) (1+ xsb)
                    (ysv-ext0 state) (1- ysb)
                    (zsv-ext0 state) (1+ zsb)))
            (t
             (psetf (dx-ext0 state) (- dx0 1 sq)
                    (dy-ext0 state) (- dy0 1 sq)
                    (dz-ext0 state) (- (1+ dz0) sq)
                    (xsv-ext0 state) (1+ xsb)
                    (ysv-ext0 state) (1+ ysb)
                    (zsv-ext0 state) (1- zsb))))
          (psetf (dx-ext1 state) (- dx0 sq2)
                 (dy-ext1 state) (- dy0 sq2)
                 (dz-ext1 state) (- dz0 sq2)
                 (xsv-ext1 state) xsb
                 (ysv-ext1 state) ysb
                 (zsv-ext1 state) zsb)
          (cond
            ((not (zerop (logand c2 1)))
             (decf (dx-ext1 state) 2)
             (incf (xsv-ext1 state) 2))
            ((not (zerop (logand c2 2)))
             (decf (dy-ext1 state) 2)
             (incf (ysv-ext1 state) 2))
            (t
             (decf (dz-ext1 state) 2)
             (incf (zsv-ext1 state) 2)))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dx2 (- dx0 sq))
          (dx4 (- dx0 1 sq2))
          (dy4 (- dy0 1 sq2))
          (dz5 (- dz0 1 sq2)))
      (psetf (dx1 state) (- dx0 1 sq)
             (dy1 state) dy1
             (dz1 state) dz1
             (dx2 state) dx2
             (dy2 state) (- dy0 1 sq)
             (dz2 state) dz1
             (dx3 state) dx2
             (dy3 state) dy1
             (dz3 state) (- dz0 1 sq)
             (dx4 state) dx4
             (dy4 state) dy4
             (dz4 state) (- dz0 sq2)
             (dx5 state) dx4
             (dy5 state) (- dy0 sq2)
             (dz5 state) dz5
             (dx6 state) (- dx0 sq2)
             (dy6 state) dy4
             (dz6 state) dz5))
    (values)))

(u:defun-inline sample (table x y z)
  (declare (optimize speed)
           (double-float x y z))
  (let ((state (make-state table x y z)))
    (cond
      ((<= (ins state) 1)
       (in1 state)
       (contribute1 state))
      ((>= (ins state) 2)
       (in2 state)
       (contribute2 state))
      (t
       (in3 state)
       (contribute3 state)))
    (contribute4 state)
    (float (* (value state) +scale+) 1f0)))

(defmethod c::%make-sampler-func ((type (eql :open-simplex-3d)))
  (let ((table (rng:shuffle 'c::rng +permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (sample table x y z))))
