(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.open-simplex-4d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:rng #:net.mfiano.lisp.algae.rng)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.open-simplex-4d)

(u:define-constant +stretch+ (/ (1- (/ (sqrt 5d0))) 4))

(u:define-constant +squish+ (/ (1- (sqrt 5d0)) 4))

(u:define-constant +scale+ (/ 30d0))

(u:define-constant +gradients+
    (let ((data '(3 1 1 1 1 3 1 1 1 1 3 1 1 1 1 3 -3 1 1 1 -1 3 1 1 -1 1 3 1 -1
                  1 1 3 3 -1 1 1 1 -3 1 1 1 -1 3 1 1 -1 1 3 -3 -1 1 1 -1 -3 1 1
                  -1 -1 3 1 -1 -1 1 3 3 1 -1 1 1 3 -1 1 1 1 -3 1 1 1 -1 3 -3 1
                  -1 1 -1 3 -1 1 -1 1 -3 1 -1 1 -1 3 3 -1 -1 1 1 -3 -1 1 1 -1 -3
                  1 1 -1 -1 3 -3 -1 -1 1 -1 -3 -1 1 -1 -1 -3 1 -1 -1 -1 3 3 1 1
                  -1 1 3 1 -1 1 1 3 -1 1 1 1 -3 -3 1 1 -1 -1 3 1 -1 -1 1 3 -1 -1
                  1 1 -3 3 -1 1 -1 1 -3 1 -1 1 -1 3 -1 1 -1 1 -3 -3 -1 1 -1 -1
                  -3 1 -1 -1 -1 3 -1 -1 -1 1 -3 3 1 -1 -1 1 3 -1 -1 1 1 -3 -1 1
                  1 -1 -3 -3 1 -1 -1 -1 3 -1 -1 -1 1 -3 -1 -1 1 -1 -3 3 -1 -1 -1
                  1 -3 -1 -1 1 -1 -3 -1 1 -1 -1 -3 -3 -1 -1 -1 -1 -3 -1 -1 -1 -1
                  -3 -1 -1 -1 -1 -3)))
      (make-array 256 :element-type 'fixnum :initial-contents data))
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
  (zsb 0 :type fixnum)
  (wsb 0 :type fixnum)
  (dx0 0d0 :type double-float)
  (dy0 0d0 :type double-float)
  (dz0 0d0 :type double-float)
  (dw0 0d0 :type double-float)
  (dx1 0d0 :type double-float)
  (dy1 0d0 :type double-float)
  (dz1 0d0 :type double-float)
  (dw1 0d0 :type double-float)
  (dx2 0d0 :type double-float)
  (dy2 0d0 :type double-float)
  (dz2 0d0 :type double-float)
  (dw2 0d0 :type double-float)
  (dx3 0d0 :type double-float)
  (dy3 0d0 :type double-float)
  (dz3 0d0 :type double-float)
  (dw3 0d0 :type double-float)
  (dx4 0d0 :type double-float)
  (dy4 0d0 :type double-float)
  (dz4 0d0 :type double-float)
  (dw4 0d0 :type double-float)
  (dx5 0d0 :type double-float)
  (dy5 0d0 :type double-float)
  (dz5 0d0 :type double-float)
  (dw5 0d0 :type double-float)
  (dx6 0d0 :type double-float)
  (dy6 0d0 :type double-float)
  (dz6 0d0 :type double-float)
  (dw6 0d0 :type double-float)
  (dx7 0d0 :type double-float)
  (dy7 0d0 :type double-float)
  (dz7 0d0 :type double-float)
  (dw7 0d0 :type double-float)
  (dx8 0d0 :type double-float)
  (dy8 0d0 :type double-float)
  (dz8 0d0 :type double-float)
  (dw8 0d0 :type double-float)
  (dx9 0d0 :type double-float)
  (dy9 0d0 :type double-float)
  (dz9 0d0 :type double-float)
  (dw9 0d0 :type double-float)
  (dx10 0d0 :type double-float)
  (dy10 0d0 :type double-float)
  (dz10 0d0 :type double-float)
  (dw10 0d0 :type double-float)
  (dx-ext0 0d0 :type double-float)
  (dy-ext0 0d0 :type double-float)
  (dz-ext0 0d0 :type double-float)
  (dw-ext0 0d0 :type double-float)
  (dx-ext1 0d0 :type double-float)
  (dy-ext1 0d0 :type double-float)
  (dz-ext1 0d0 :type double-float)
  (dw-ext1 0d0 :type double-float)
  (dx-ext2 0d0 :type double-float)
  (dy-ext2 0d0 :type double-float)
  (dz-ext2 0d0 :type double-float)
  (dw-ext2 0d0 :type double-float)
  (xsv-ext0 0 :type fixnum)
  (ysv-ext0 0 :type fixnum)
  (zsv-ext0 0 :type fixnum)
  (wsv-ext0 0 :type fixnum)
  (xsv-ext1 0 :type fixnum)
  (ysv-ext1 0 :type fixnum)
  (zsv-ext1 0 :type fixnum)
  (wsv-ext1 0 :type fixnum)
  (xsv-ext2 0 :type fixnum)
  (ysv-ext2 0 :type fixnum)
  (zsv-ext2 0 :type fixnum)
  (wsv-ext2 0 :type fixnum)
  (xins 0d0 :type double-float)
  (yins 0d0 :type double-float)
  (zins 0d0 :type double-float)
  (wins 0d0 :type double-float)
  (ins 0d0 :type double-float)
  (value 0d0 :type double-float))

(u:defun-inline make-state (table x y z w)
  (let* ((stretch-offset (* (+ x y z w) +stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (zs (+ z stretch-offset))
         (ws (+ w stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (zsb (floor zs))
         (wsb (floor ws))
         (squish-offset (* (+ xsb ysb zsb wsb) +squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (dz0 (- z (+ zsb squish-offset)))
         (dw0 (- w (+ wsb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb))
         (zins (- zs zsb))
         (wins (- ws wsb)))
    (declare (c:f50 xs ys zs ws))
    (%make-state :table table
                 :xsb xsb
                 :ysb ysb
                 :zsb zsb
                 :wsb wsb
                 :dx0 dx0
                 :dy0 dy0
                 :dz0 dz0
                 :dw0 dw0
                 :xins xins
                 :yins yins
                 :zins zins
                 :wins wins
                 :ins (+ xins yins zins wins))))

(u:defun-inline extrapolate (table xsb ysb zsb wsb dx dy dz dw)
  (let ((index (logand (c:pget table wsb zsb ysb xsb) 252)))
    (+ (* (aref +gradients+ index) dx)
       (* (aref +gradients+ (1+ index)) dy)
       (* (aref +gradients+ (+ index 2)) dz)
       (* (aref +gradients+ (+ index 3)) dw))))

(u:defun-inline contribute (state dx dy dz dw xsb ysb zsb wsb)
  (let ((a (- 2 (* dx dx) (* dy dy) (* dz dz) (* dw dw))))
    (when (plusp a)
      (incf (value state)
            (* (expt a 4)
               (extrapolate (table state) xsb ysb zsb wsb dx dy dz dw))))
    (values)))

(defun contribute1 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state))
        (wsb (wsb state)))
    (contribute state (dx0 state) (dy0 state) (dz0 state) (dw0 state) xsb ysb
                zsb wsb)
    (contribute state (dx1 state) (dy1 state) (dz1 state) (dw1 state) (1+ xsb)
                ysb zsb wsb)
    (contribute state (dx2 state) (dy2 state) (dz2 state) (dw2 state) xsb
                (1+ ysb) zsb wsb)
    (contribute state (dx3 state) (dy3 state) (dz3 state) (dw3 state) xsb ysb
                (1+ zsb) wsb)
    (contribute state (dx4 state) (dy4 state) (dz4 state) (dw4 state) xsb ysb
                zsb (1+ wsb))))

(defun contribute2 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state))
        (wsb (wsb state)))
    (contribute state (dx4 state) (dy4 state) (dz4 state) (dw4 state) (1+ xsb)
                (1+ ysb) (1+ zsb) wsb)
    (contribute state (dx3 state) (dy3 state) (dz3 state) (dw3 state) (1+ xsb)
                (1+ ysb) zsb (1+ wsb))
    (contribute state (dx2 state) (dy2 state) (dz2 state) (dw2 state) (1+ xsb)
                ysb (1+ zsb) (1+ wsb))
    (contribute state (dx1 state) (dy1 state) (dz1 state) (dw1 state) xsb
                (1+ ysb) (1+ zsb) (1+ wsb))
    (contribute state (dx0 state) (dy0 state) (dz0 state) (dw0 state) (1+ xsb)
                (1+ ysb) (1+ zsb) (1+ wsb))))

(defun contribute3 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state))
        (wsb (wsb state)))
    (contribute state (dx1 state) (dy1 state) (dz1 state) (dw1 state) (1+ xsb)
                ysb zsb wsb)
    (contribute state (dx2 state) (dy2 state) (dz2 state) (dw2 state) xsb
                (1+ ysb) zsb wsb)
    (contribute state (dx3 state) (dy3 state) (dz3 state) (dw3 state) xsb ysb
                (1+ zsb) wsb)
    (contribute state (dx4 state) (dy4 state) (dz4 state) (dw4 state) xsb ysb
                zsb (1+ wsb))
    (contribute state (dx5 state) (dy5 state) (dz5 state) (dw5 state) (1+ xsb)
                (1+ ysb) zsb wsb)
    (contribute state (dx6 state) (dy6 state) (dz6 state) (dw6 state) (1+ xsb)
                ysb (1+ zsb) wsb)
    (contribute state (dx7 state) (dy7 state) (dz7 state) (dw7 state) (1+ xsb)
                ysb zsb (1+ wsb))
    (contribute state (dx8 state) (dy8 state) (dz8 state) (dw8 state) xsb
                (1+ ysb) (1+ zsb) wsb)
    (contribute state (dx9 state) (dy9 state) (dz9 state) (dw9 state) xsb
                (1+ ysb) zsb (1+ wsb))
    (contribute state (dx10 state) (dy10 state) (dz10 state) (dw10 state) xsb
                ysb (1+ zsb) (1+ wsb))))

(defun contribute4 (state)
  (let ((xsb (xsb state))
        (ysb (ysb state))
        (zsb (zsb state))
        (wsb (wsb state)))
    (contribute state (dx4 state) (dy4 state) (dz4 state) (dw4 state) (1+ xsb)
                (1+ ysb) (1+ zsb) wsb)
    (contribute state (dx3 state) (dy3 state) (dz3 state) (dw3 state) (1+ xsb)
                (1+ ysb) zsb (1+ wsb))
    (contribute state (dx2 state) (dy2 state) (dz2 state) (dw2 state) (1+ xsb)
                ysb (1+ zsb) (1+ wsb))
    (contribute state (dx1 state) (dy1 state) (dz1 state) (dw1 state) xsb
                (1+ ysb) (1+ zsb) (1+ wsb))
    (contribute state (dx5 state) (dy5 state) (dz5 state) (dw5 state) (1+ xsb)
                (1+ ysb) zsb wsb)
    (contribute state (dx6 state) (dy6 state) (dz6 state) (dw6 state) (1+ xsb)
                ysb (1+ zsb) wsb)
    (contribute state (dx7 state) (dy7 state) (dz7 state) (dw7 state) (1+ xsb)
                ysb zsb (1+ wsb))
    (contribute state (dx8 state) (dy8 state) (dz8 state) (dw8 state) xsb
                (1+ ysb) (1+ zsb) wsb)
    (contribute state (dx9 state) (dy9 state) (dz9 state) (dw9 state) xsb
                (1+ ysb) zsb (1+ wsb))
    (contribute state (dx10 state) (dy10 state) (dz10 state) (dw10 state) xsb
                ysb (1+ zsb) (1+ wsb))))

(defun contribute5 (state)
  (contribute state (dx-ext0 state) (dy-ext0 state) (dz-ext0 state)
              (dw-ext0 state) (xsv-ext0 state) (ysv-ext0 state) (zsv-ext0 state)
              (wsv-ext0 state))
  (contribute state (dx-ext1 state) (dy-ext1 state) (dz-ext1 state)
              (dw-ext1 state) (xsv-ext1 state) (ysv-ext1 state) (zsv-ext1 state)
              (wsv-ext1 state))
  (contribute state (dx-ext2 state) (dy-ext2 state) (dz-ext2 state)
              (dw-ext2 state) (xsv-ext2 state) (ysv-ext2 state) (zsv-ext2 state)
              (wsv-ext2 state)))

(defun in1 (state)
  (let* ((point-a 1)
         (point-b 2)
         (score-a (xins state))
         (score-b (yins state))
         (zins (zins state))
         (wins (wins state))
         (uins (- 1 (ins state)))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (wsb (wsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (dw0 (dw0 state))
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
    (cond
      ((and (>= score-a score-b)
            (> wins score-b))
       (psetf score-b wins
              point-b 8))
      ((and (< score-a score-b)
            (> wins score-a))
       (psetf score-a wins
              point-a 8)))
    (if (or (> uins score-a)
            (> uins score-b))
        (let ((c (if (> score-b score-a) point-b point-a)))
          (if (zerop (logand c 1))
              (psetf (xsv-ext0 state) (1- xsb)
                     (xsv-ext1 state) xsb
                     (xsv-ext2 state) xsb
                     (dx-ext0 state) (1+ dx0)
                     (dx-ext1 state) dx0
                     (dx-ext2 state) dx0)
              (let ((xsv (1+ xsb))
                    (dx (1- dx0)))
                (psetf (xsv-ext0 state) xsv
                       (xsv-ext1 state) xsv
                       (xsv-ext2 state) xsv
                       (dx-ext0 state) dx
                       (dx-ext1 state) dx
                       (dx-ext2 state) dx)))
          (cond
            ((zerop (logand c 2))
             (psetf (ysv-ext0 state) ysb
                    (ysv-ext1 state) ysb
                    (ysv-ext2 state) ysb
                    (dy-ext0 state) dy0
                    (dy-ext1 state) dy0
                    (dy-ext2 state) dy0)
             (cond
               ((= (logand c 1) 1)
                (decf (ysv-ext0 state))
                (incf (dy-ext0 state)))
               (t
                (decf (ysv-ext1 state))
                (incf (dy-ext1 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (1- dy0)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (ysv-ext2 state) ysv
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))))
          (cond
            ((zerop (logand c 4))
             (psetf (zsv-ext0 state) zsb
                    (zsv-ext1 state) zsb
                    (zsv-ext2 state) zsb
                    (dz-ext0 state) dz0
                    (dz-ext1 state) dz0
                    (dz-ext2 state) dz0)
             (cond
               ((not (zerop (logand c 3)))
                (unless (= (logand c 3) 3)
                  (decf (zsv-ext1 state))
                  (incf (dz-ext1 state))))
               (t
                (decf (zsv-ext2 state))
                (incf (dz-ext2 state)))))
            (t
             (let ((zsv (1+ zsb))
                   (dz (1- dz0)))
               (psetf (zsv-ext0 state) zsv
                      (zsv-ext1 state) zsv
                      (zsv-ext2 state) zsv
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))))
          (if (zerop (logand c 8))
              (psetf (wsv-ext0 state) wsb
                     (wsv-ext1 state) wsb
                     (wsv-ext2 state) (1- wsb)
                     (dw-ext0 state) dw0
                     (dw-ext1 state) dw0
                     (dw-ext2 state) (1+ dw0))
              (let ((wsv (1+ wsb))
                    (dw (1- dw0)))
                (psetf (wsv-ext0 state) wsv
                       (wsv-ext1 state) wsv
                       (wsv-ext2 state) wsv
                       (dw-ext0 state) dw
                       (dw-ext1 state) dw
                       (dw-ext2 state) dw))))
        (let ((c (logior point-a point-b)))
          (if (zerop (logand c 1))
              (psetf (xsv-ext0 state) xsb
                     (xsv-ext1 state) (1- xsb)
                     (xsv-ext2 state) xsb
                     (dx-ext0 state) (- dx0 sq2)
                     (dx-ext1 state) (- (1+ dx0) sq)
                     (dx-ext2 state) (- dx0 sq))
              (let ((xsv (1+ xsb))
                    (dx (- dx0 1 sq)))
                (psetf (xsv-ext0 state) xsv
                       (xsv-ext1 state) xsv
                       (xsv-ext2 state) xsv
                       (dx-ext0 state) (- dx0 1 sq2)
                       (dx-ext1 state) dx
                       (dx-ext2 state) dx)))
          (cond
            ((zerop (logand c 2))
             (let ((dy (- dy0 sq)))
               (psetf (ysv-ext0 state) ysb
                      (ysv-ext1 state) ysb
                      (ysv-ext2 state) ysb
                      (dy-ext0 state) (- dy0 sq2)
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))
             (cond
               ((= (logand c 1) 1)
                (decf (ysv-ext1 state))
                (incf (dy-ext1 state)))
               (t
                (decf (ysv-ext2 state))
                (incf (dy-ext2 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (ysv-ext2 state) ysv
                      (dy-ext0 state) (- dy0 1 sq2)
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))))
          (cond
            ((zerop (logand c 4))
             (let ((dz (- dz0 sq)))
               (psetf (zsv-ext0 state) zsb
                      (zsv-ext1 state) zsb
                      (zsv-ext2 state) zsb
                      (dz-ext0 state) (- dz0 sq2)
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))
             (cond
               ((= (logand c 3) 3)
                (decf (zsv-ext1 state))
                (incf (dz-ext1 state)))
               (t
                (decf (zsv-ext2 state))
                (incf (dz-ext2 state)))))
            (t
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq)))
               (psetf (zsv-ext0 state) zsv
                      (zsv-ext1 state) zsv
                      (zsv-ext2 state) zsv
                      (dz-ext0 state) (- dz0 1 sq2)
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))))
          (cond
            ((zerop (logand c 8))
             (psetf (wsv-ext0 state) wsb
                    (wsv-ext1 state) wsb
                    (wsv-ext2 state) (1- wsb)
                    (dw-ext0 state) (- dw0 sq2)
                    (dw-ext1 state) (- dw0 sq)
                    (dw-ext2 state) (- (1+ dw0) sq)))
            (t
             (let ((wsv (1+ wsb))
                   (dw (- dw0 1 sq)))
               (psetf (wsv-ext0 state) wsv
                      (wsv-ext1 state) wsv
                      (wsv-ext2 state) wsv
                      (dw-ext0 state) (- dw0 1 sq2)
                      (dw-ext1 state) dw
                      (dw-ext2 state) dw))))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dw1 (- dw0 sq))
          (dx2 (- dx0 sq)))
      (psetf (dx1 state) (- dx0 1 sq)
             (dy1 state) dy1
             (dz1 state) dz1
             (dw1 state) dw1
             (dx2 state) dx2
             (dy2 state) (- dy0 1 sq)
             (dz2 state) dz1
             (dw2 state) dw1
             (dx3 state) dx2
             (dy3 state) dy1
             (dz3 state) (- dz0 1 sq)
             (dw3 state) dw1
             (dx4 state) dx2
             (dy4 state) dy1
             (dz4 state) dz1
             (dw4 state) (- dw0 1 sq)))
    (values)))

(defun in2 (state)
  (let* ((point-a 14)
         (point-b 13)
         (score-a (xins state))
         (score-b (yins state))
         (zins (zins state))
         (wins (wins state))
         (uins (- 4 (ins state)))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (wsb (wsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (dw0 (dw0 state))
         (sq2 #.(* +squish+ 2))
         (sq3 #.(* +squish+ 3))
         (sq4 #.(* +squish+ 4)))
    (cond
      ((and (<= score-a score-b)
            (< zins score-b))
       (psetf score-b zins
              point-b 11))
      ((and (> score-a score-b)
            (< zins score-a))
       (psetf score-a zins
              point-a 11)))
    (cond
      ((and (<= score-a score-b)
            (< wins score-b))
       (psetf score-b wins
              point-b 7))
      ((and (> score-a score-b)
            (< wins score-a))
       (psetf score-a wins
              point-a 7)))
    (if (or (< uins score-a)
            (< uins score-b))
        (let ((c (if (< score-b score-a) point-b point-a)))
          (if (not (zerop (logand c 1)))
              (let ((xsv (1+ xsb))
                    (dx (- dx0 1 sq4)))
                (psetf (xsv-ext0 state) (+ xsb 2)
                       (xsv-ext1 state) xsv
                       (xsv-ext2 state) xsv
                       (dx-ext0 state) (- dx0 2 sq4)
                       (dx-ext1 state) dx
                       (dx-ext2 state) dx))
              (let ((dx (- dx0 sq4)))
                (psetf (xsv-ext0 state) xsb
                       (xsv-ext1 state) xsb
                       (xsv-ext2 state) xsb
                       (dx-ext0 state) dx
                       (dx-ext1 state) dx
                       (dx-ext2 state) dx)))
          (cond
            ((not (zerop (logand c 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq4)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (ysv-ext2 state) ysv
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))
             (cond
               ((not (zerop (logand c 1)))
                (incf (ysv-ext1 state))
                (decf (dy-ext1 state)))
               (t
                (incf (ysv-ext0 state))
                (decf (dy-ext0 state)))))
            (t
             (let ((dy (- dy0 sq4)))
               (psetf (ysv-ext0 state) ysb
                      (ysv-ext1 state) ysb
                      (ysv-ext2 state) ysb
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))))
          (cond
            ((not (zerop (logand c 4)))
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq4)))
               (psetf (zsv-ext0 state) zsv
                      (zsv-ext1 state) zsv
                      (zsv-ext2 state) zsv
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))
             (cond
               ((not (= (logand c 3) 3))
                (unless (zerop (logand c 3))
                  (incf (zsv-ext1 state))
                  (decf (dz-ext1 state))))
               (t
                (incf (zsv-ext2 state))
                (decf (dz-ext2 state)))))
            (t
             (let ((dz (- dz0 sq4)))
               (psetf (zsv-ext0 state) zsb
                      (zsv-ext1 state) zsb
                      (zsv-ext2 state) zsb
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))))
          (if (not (zerop (logand c 8)))
              (let ((wsv (1+ wsb))
                    (dw (- dw0 1 sq4)))
                (psetf (wsv-ext0 state) wsv
                       (wsv-ext1 state) wsv
                       (wsv-ext2 state) (+ wsb 2)
                       (dw-ext0 state) dw
                       (dw-ext1 state) dw
                       (dw-ext2 state) (- dw0 2 sq4)))
              (let ((dw (- dw0 sq4)))
                (psetf (wsv-ext0 state) wsb
                       (wsv-ext1 state) wsb
                       (wsv-ext2 state) wsb
                       (dw-ext0 state) dw
                       (dw-ext1 state) dw
                       (dw-ext2 state) dw))))
        (let ((c (logand point-a point-b)))
          (if (not (zerop (logand c 1)))
              (let ((xsv (1+ xsb)))
                (psetf (xsv-ext0 state) xsv
                       (xsv-ext1 state) (+ xsb 2)
                       (xsv-ext2 state) xsv
                       (dx-ext0 state) (- dx0 1 sq2)
                       (dx-ext1 state) (- dx0 2 sq3)
                       (dx-ext2 state) (- dx0 1 sq3)))
              (let ((dx (- dx0 sq3)))
                (psetf (xsv-ext0 state) xsb
                       (xsv-ext1 state) xsb
                       (xsv-ext2 state) xsb
                       (dx-ext0 state) (- dx0 sq2)
                       (dx-ext1 state) dx
                       (dx-ext2 state) dx)))
          (cond
            ((not (zerop (logand c 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq3)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (ysv-ext2 state) ysv
                      (dy-ext0 state) (- dy0 1 sq2)
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))
             (cond
               ((not (zerop (logand c 1)))
                (incf (ysv-ext2 state))
                (decf (dy-ext2 state)))
               (t
                (incf (ysv-ext1 state))
                (decf (dy-ext1 state)))))
            (t
             (let ((dy (- dy0 sq3)))
               (psetf (ysv-ext0 state) ysb
                      (ysv-ext1 state) ysb
                      (ysv-ext2 state) ysb
                      (dy-ext0 state) (- dy0 sq2)
                      (dy-ext1 state) dy
                      (dy-ext2 state) dy))))
          (cond
            ((not (zerop (logand c 4)))
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq3)))
               (psetf (zsv-ext0 state) zsv
                      (zsv-ext1 state) zsv
                      (zsv-ext2 state) zsv
                      (dz-ext0 state) (- dz0 1 sq2)
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))
             (cond
               ((not (zerop (logand c 3)))
                (incf (zsv-ext2 state))
                (decf (dz-ext2 state)))
               (t
                (incf (zsv-ext1 state))
                (decf (dz-ext1 state)))))
            (t
             (let ((dz (- dz0 sq3)))
               (psetf (zsv-ext0 state) zsb
                      (zsv-ext1 state) zsb
                      (zsv-ext2 state) zsb
                      (dz-ext0 state) (- dz0 sq2)
                      (dz-ext1 state) dz
                      (dz-ext2 state) dz))))
          (cond
            ((not (zerop (logand c 8)))
             (let  ((wsv (1+ wsb)))
               (psetf (wsv-ext0 state) wsv
                      (wsv-ext1 state) wsv
                      (wsv-ext2 state) (+ wsb 2)
                      (dw-ext0 state) (- dw0 1 sq2)
                      (dw-ext1 state) (- dw0 1 sq3)
                      (dw-ext2 state) (- dw0 2 sq3))))
            (t
             (let ((dw (- dw0 sq3)))
               (psetf (wsv-ext0 state) wsb
                      (wsv-ext1 state) wsb
                      (wsv-ext2 state) wsb
                      (dw-ext0 state) (- dw0 sq2)
                      (dw-ext1 state) dw
                      (dw-ext2 state) dw))))))
    (let ((dw3 (- dw0 1 sq3))
          (dx4 (- dx0 1 sq3))
          (dy4 (- dy0 1 sq3))
          (dz4 (- dz0 1 sq3)))
      (psetf (dx4 state) dx4
             (dy4 state) dy4
             (dz4 state) dz4
             (dw4 state) (- dw0 sq3)
             (dx3 state) dx4
             (dy3 state) dy4
             (dz3 state) (- dz0 sq3)
             (dw3 state) dw3
             (dx2 state) dx4
             (dy2 state) (- dy0 sq3)
             (dz2 state) dz4
             (dw2 state) dw3
             (dx1 state) (- dx0 sq3)
             (dy1 state) dy4
             (dz1 state) dz4
             (dw1 state) dw3
             (dx0 state) (- dx0 1 sq4)
             (dy0 state) (- dy0 1 sq4)
             (dz0 state) (- dz0 1 sq4)
             (dw0 state) (- dw0 1 sq4)))
    (values)))

(defun in3 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-bigger-p t)
         (b-bigger-p t)
         (xins (xins state))
         (yins (yins state))
         (zins (zins state))
         (wins (wins state))
         (ins (ins state))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (wsb (wsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (dw0 (dw0 state))
         (sq +squish+)
         (sq2 #.(* +squish+ 2))
         (sq3 #.(* +squish+ 3)))
    (if (> (+ xins yins) (+ zins wins))
        (psetf score-a (+ xins yins)
               point-a 3)
        (psetf score-a (+ zins wins)
               point-a 12))
    (if (> (+ xins zins) (+ yins wins))
        (psetf score-b (+ xins zins)
               point-b 5)
        (psetf score-b (+ yins wins)
               point-b 10))
    (if (> (+ xins wins) (+ yins zins))
        (let ((score (+ xins wins)))
          (cond
            ((and (>= score-a score-b)
                  (> score score-b))
             (psetf score-b score
                    point-b 9))
            ((and (< score-a score-b)
                  (> score score-a))
             (psetf score-a score
                    point-a 9))))
        (let ((score (+ yins zins)))
          (cond
            ((and (>= score-a score-b)
                  (> score score-b))
             (psetf score-b score
                    point-b 6))
            ((and (< score-a score-b)
                  (> score score-a))
             (psetf score-a score
                    point-a 6)))))
    (let ((p1 (+ (- 2 ins) xins)))
      (cond
        ((and (>= score-a score-b)
              (> p1 score-b))
         (psetf score-b p1
                point-b 1
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p1 score-a))
         (psetf score-a p1
                point-a 1
                a-bigger-p nil))))
    (let ((p2 (+ (- 2 ins) yins)))
      (cond
        ((and (>= score-a score-b)
              (> p2 score-b))
         (psetf score-b p2
                point-b 2
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p2 score-a))
         (psetf score-a p2
                point-a 2
                a-bigger-p nil))))
    (let ((p3 (+ (- 2 ins) zins)))
      (cond
        ((and (>= score-a score-b)
              (> p3 score-b))
         (psetf score-b p3
                point-b 4
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p3 score-a))
         (psetf score-a p3
                point-a 4
                a-bigger-p nil))))
    (let ((p4 (+ (- 2 ins) wins)))
      (cond
        ((and (>= score-a score-b)
              (> p4 score-b))
         (psetf score-b p4
                point-b 8
                b-bigger-p nil))
        ((and (< score-a score-b)
              (> p4 score-a))
         (psetf score-a p4
                point-a 8
                a-bigger-p nil))))
    (if (eq a-bigger-p b-bigger-p)
        (if a-bigger-p
            (let ((c1 (logior point-a point-b))
                  (c2 (logand point-a point-b)))
              (if (zerop (logand c1 1))
                  (psetf (xsv-ext0 state) xsb
                         (xsv-ext1 state) (1- xsb)
                         (dx-ext0 state) (- dx0 sq3)
                         (dx-ext1 state) (- (1+ dx0) sq2))
                  (let ((xsv (1+ xsb)))
                    (psetf (xsv-ext0 state) xsv
                           (xsv-ext1 state) xsv
                           (dx-ext0 state) (- dx0 1 sq3)
                           (dx-ext1 state) (- dx0 1 sq2))))
              (if (zerop (logand c1 2))
                  (psetf (ysv-ext0 state) ysb
                         (ysv-ext1 state) (1- ysb)
                         (dy-ext0 state) (- dy0 sq3)
                         (dy-ext1 state) (- (1+ dy0) sq2))
                  (let ((ysv (1+ ysb)))
                    (psetf (ysv-ext0 state) ysv
                           (ysv-ext1 state) ysv
                           (dy-ext0 state) (- dy0 1 sq3)
                           (dy-ext1 state) (- dy0 1 sq2))))
              (if (zerop (logand c1 4))
                  (psetf (zsv-ext0 state) zsb
                         (zsv-ext1 state) (1- zsb)
                         (dz-ext0 state) (- dz0 sq3)
                         (dz-ext1 state) (- (1+ dz0) sq2))
                  (let ((zsv (1+ zsb)))
                    (psetf (zsv-ext0 state) zsv
                           (zsv-ext1 state) zsv
                           (dz-ext0 state) (- dz0 1 sq3)
                           (dz-ext1 state) (- dz0 1 sq2))))
              (if (zerop (logand c1 8))
                  (psetf (wsv-ext0 state) wsb
                         (wsv-ext1 state) (1- wsb)
                         (dw-ext0 state) (- dw0 sq3)
                         (dw-ext1 state) (- (1+ dw0) sq2))
                  (let ((wsv (1+ wsb)))
                    (psetf (wsv-ext0 state) wsv
                           (wsv-ext1 state) wsv
                           (dw-ext0 state) (- dw0 1 sq3)
                           (dw-ext1 state) (- dw0 1 sq2))))
              (psetf (xsv-ext2 state) xsb
                     (ysv-ext2 state) ysb
                     (zsv-ext2 state) zsb
                     (wsv-ext2 state) wsb
                     (dx-ext2 state) (- dx0 sq2)
                     (dy-ext2 state) (- dy0 sq2)
                     (dz-ext2 state) (- dz0 sq2)
                     (dw-ext2 state) (- dw0 sq2))
              (cond
                ((not (zerop (logand c2 1)))
                 (incf (xsv-ext2 state) 2)
                 (decf (dx-ext2 state) 2))
                ((not (zerop (logand c2 2)))
                 (incf (ysv-ext2 state) 2)
                 (decf (dy-ext2 state) 2))
                ((not (zerop (logand c2 4)))
                 (incf (zsv-ext2 state) 2)
                 (decf (dz-ext2 state) 2))
                (t
                 (incf (wsv-ext2 state) 2)
                 (decf (dw-ext2 state) 2))))
            (let ((c (logior point-a point-b)))
              (psetf (xsv-ext2 state) xsb
                     (ysv-ext2 state) ysb
                     (zsv-ext2 state) zsb
                     (wsv-ext2 state) wsb
                     (dx-ext2 state) dx0
                     (dy-ext2 state) dy0
                     (dz-ext2 state) dz0
                     (dw-ext2 state) dw0)
              (if (zerop (logand c 1))
                  (psetf (xsv-ext0 state) (1- xsb)
                         (xsv-ext1 state) xsb
                         (dx-ext0 state) (- (1+ dx0) sq)
                         (dx-ext1 state) (- dx0 sq))
                  (let ((xsv (1+ xsb))
                        (dx (- dx0 1 sq)))
                    (psetf (xsv-ext0 state) xsv
                           (xsv-ext1 state) xsv
                           (dx-ext0 state) dx
                           (dx-ext1 state) dx)))
              (cond
                ((zerop (logand c 2))
                 (let ((dy (- dy0 sq)))
                   (psetf (ysv-ext0 state) ysb
                          (ysv-ext1 state) ysb
                          (dy-ext0 state) dy
                          (dy-ext1 state) dy))
                 (cond
                   ((= (logand c 1) 1)
                    (decf (ysv-ext0 state))
                    (incf (dy-ext0 state)))
                   (t
                    (decf (ysv-ext1 state))
                    (incf (dy-ext1 state)))))
                (t
                 (let ((ysv (1+ ysb))
                       (dy (- dy0 1 sq)))
                   (psetf (ysv-ext0 state) ysv
                          (ysv-ext1 state) ysv
                          (dy-ext0 state) dy
                          (dy-ext1 state) dy))))
              (cond
                ((zerop (logand c 4))
                 (let ((dz (- dz0 sq)))
                   (psetf (zsv-ext0 state) zsb
                          (zsv-ext1 state) zsb
                          (dz-ext0 state) dz
                          (dz-ext1 state) dz))
                 (cond
                   ((= (logand c 3) 3)
                    (decf (zsv-ext0 state))
                    (incf (dz-ext0 state)))
                   (t
                    (decf (zsv-ext1 state))
                    (incf (dz-ext1 state)))))
                (t
                 (let ((zsv (1+ zsb))
                       (dz (- dz0 1 sq)))
                   (psetf (zsv-ext0 state) zsv
                          (zsv-ext1 state) zsv
                          (dz-ext0 state) dz
                          (dz-ext1 state) dz))))
              (cond
                ((zerop (logand c 8))
                 (psetf (wsv-ext0 state) wsb
                        (wsv-ext1 state) (1- wsb)
                        (dw-ext0 state) (- dw0 sq)
                        (dw-ext1 state) (- (1+ dw0) sq)))
                (t
                 (let ((wsv (1+ wsb))
                       (dw (- dw0 1 sq)))
                   (psetf (wsv-ext0 state) wsv
                          (wsv-ext1 state) wsv
                          (dw-ext0 state) dw
                          (dw-ext1 state) dw))))))
        (let ((c1 (if a-bigger-p point-a point-b))
              (c2 (if a-bigger-p point-b point-a)))
          (if (zerop (logand c1 1))
              (psetf (xsv-ext0 state) (1- xsb)
                     (xsv-ext1 state) xsb
                     (dx-ext0 state) (- (1+ dx0) sq)
                     (dx-ext1 state) (- dx0 sq))
              (let ((xsv (1+ xsb))
                    (dx (- dx0 1 sq)))
                (psetf (xsv-ext0 state) xsv
                       (xsv-ext1 state) xsv
                       (dx-ext0 state) dx
                       (dx-ext1 state) dx)))
          (cond
            ((zerop (logand c1 2))
             (let ((dy (- dy0 sq)))
               (psetf (ysv-ext0 state) ysb
                      (ysv-ext1 state) ysb
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))
             (cond
               ((= (logand c1 1) 1)
                (decf (ysv-ext0 state))
                (incf (dy-ext0 state)))
               (t
                (decf (ysv-ext1 state))
                (incf (dy-ext1 state)))))
            (t
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))))
          (cond
            ((zerop (logand c1 4))
             (let ((dz (- dz0 sq)))
               (psetf (zsv-ext0 state) zsb
                      (zsv-ext1 state) zsb
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz))
             (cond
               ((= (logand c1 3) 3)
                (decf (zsv-ext0 state))
                (incf (dz-ext0 state)))
               (t
                (decf (zsv-ext1 state))
                (incf (dz-ext1 state)))))
            (t
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq)))
               (psetf (zsv-ext0 state) zsv
                      (zsv-ext1 state) zsv
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz))))
          (if (zerop (logand c1 8))
              (psetf (wsv-ext0 state) wsb
                     (wsv-ext1 state) (1- wsb)
                     (dw-ext0 state) (- dw0 sq)
                     (dw-ext1 state) (- (1+ dw0) sq))
              (let ((wsv (1+ wsb))
                    (dw (- dw0 1 sq)))
                (psetf (wsv-ext0 state) wsv
                       (wsv-ext1 state) wsv
                       (dw-ext0 state) dw
                       (dw-ext1 state) dw)))
          (psetf (xsv-ext2 state) xsb
                 (ysv-ext2 state) ysb
                 (zsv-ext2 state) zsb
                 (wsv-ext2 state) wsb
                 (dx-ext2 state) (- dx0 sq2)
                 (dy-ext2 state) (- dy0 sq2)
                 (dz-ext2 state) (- dz0 sq2)
                 (dw-ext2 state) (- dw0 sq2))
          (cond
            ((not (zerop (logand c2 1)))
             (incf (xsv-ext2 state) 2)
             (decf (dx-ext2 state) 2))
            ((not (zerop (logand c2 2)))
             (incf (ysv-ext2 state) 2)
             (decf (dy-ext2 state) 2))
            ((not (zerop (logand c2 4)))
             (incf (zsv-ext2 state) 2)
             (decf (dz-ext2 state) 2))
            (t
             (incf (wsv-ext2 state) 2)
             (decf (dw-ext2 state) 2)))))
    (let ((dy1 (- dy0 sq))
          (dz1 (- dz0 sq))
          (dw1 (- dw0 sq))
          (dx2 (- dx0 sq))
          (dx5 (- dx0 1 sq2))
          (dy5 (- dy0 1 sq2))
          (dz5 (- dz0 sq2))
          (dw5 (- dw0 sq2))
          (dy6 (- dy0 sq2))
          (dz6 (- dz0 1 sq2))
          (dw7 (- dw0 1 sq2))
          (dx8 (- dx0 sq2)))
      (psetf (dx1 state) (- dx0 1 sq)
             (dy1 state) dy1
             (dz1 state) dz1
             (dw1 state) dw1
             (dx2 state) dx2
             (dy2 state) (- dy0 1 sq)
             (dz2 state) dz1
             (dw2 state) dw1
             (dx3 state) dx2
             (dy3 state) dy1
             (dz3 state) (- dz0 1 sq)
             (dw3 state) dw1
             (dx4 state) dx2
             (dy4 state) dy1
             (dz4 state) dz1
             (dw4 state) (- dw0 1 sq)
             (dx5 state) dx5
             (dy5 state) dy5
             (dz5 state) dz5
             (dw5 state) dw5
             (dx6 state) dx5
             (dy6 state) dy6
             (dz6 state) dz6
             (dw6 state) dw5
             (dx7 state) dx5
             (dy7 state) dy6
             (dz7 state) dz5
             (dw7 state) dw7
             (dx8 state) dx8
             (dy8 state) dy5
             (dz8 state) dz6
             (dw8 state) dw5
             (dx9 state) dx8
             (dy9 state) dy5
             (dz9 state) dz5
             (dw9 state) dw7
             (dx10 state) dx8
             (dy10 state) dy6
             (dz10 state) dz6
             (dw10 state) dw7))
    (values)))

(defun in4 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-bigger-p t)
         (b-bigger-p t)
         (xins (xins state))
         (yins (yins state))
         (zins (zins state))
         (wins (wins state))
         (ins (ins state))
         (xsb (xsb state))
         (ysb (ysb state))
         (zsb (zsb state))
         (wsb (wsb state))
         (dx0 (dx0 state))
         (dy0 (dy0 state))
         (dz0 (dz0 state))
         (dw0 (dw0 state))
         (sq +squish+)
         (sq2 #.(* +squish+ 2))
         (sq3 #.(* +squish+ 3))
         (sq4 #.(* +squish+ 4)))
    (if (< (+ xins yins) (+ zins wins))
        (psetf score-a (+ xins yins)
               point-a 12)
        (psetf score-a (+ zins wins)
               point-a 3))
    (if (< (+ xins zins) (+ yins wins))
        (psetf score-b (+ xins zins)
               point-b 10)
        (psetf score-b (+ yins wins)
               point-b 5))
    (if (< (+ xins wins) (+ yins zins))
        (let ((score (+ xins wins)))
          (cond
            ((and (<= score-a score-b)
                  (< score score-b))
             (psetf score-b score
                    point-b 6))
            ((and (> score-a score-b)
                  (< score score-a))
             (psetf score-a score
                    point-a 6))))
        (let ((score (+ yins zins)))
          (cond
            ((and (<= score-a score-b)
                  (< score score-b))
             (psetf score-b score
                    point-b 9))
            ((and (> score-a score-b)
                  (< score score-a))
             (psetf score-a score
                    point-a 9)))))
    (let ((p1 (+ (- 3 ins) xins)))
      (cond
        ((and (<= score-a score-b)
              (< p1 score-b))
         (psetf score-b p1
                point-b 14
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p1 score-a))
         (psetf score-a p1
                point-a 14
                a-bigger-p nil))))
    (let ((p2 (+ (- 3 ins) yins)))
      (cond
        ((and (<= score-a score-b)
              (< p2 score-b))
         (psetf score-b p2
                point-b 13
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p2 score-a))
         (psetf score-a p2
                point-a 13
                a-bigger-p nil))))
    (let ((p3 (+ (- 3 ins) zins)))
      (cond
        ((and (<= score-a score-b)
              (< p3 score-b))
         (psetf score-b p3
                point-b 11
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p3 score-a))
         (psetf score-a p3
                point-a 11
                a-bigger-p nil))))
    (let ((p4 (+ (- 3 ins) wins)))
      (cond
        ((and (<= score-a score-b)
              (< p4 score-b))
         (psetf score-b p4
                point-b 7
                b-bigger-p nil))
        ((and (> score-a score-b)
              (< p4 score-a))
         (psetf score-a p4
                point-a 7
                a-bigger-p nil))))
    (if (eq a-bigger-p b-bigger-p)
        (if a-bigger-p
            (let ((c1 (logand point-a point-b))
                  (c2 (logior point-a point-b)))
              (psetf (xsv-ext0 state) xsb
                     (xsv-ext1 state) xsb
                     (ysv-ext0 state) ysb
                     (ysv-ext1 state) ysb
                     (zsv-ext0 state) zsb
                     (zsv-ext1 state) zsb
                     (wsv-ext0 state) wsb
                     (wsv-ext1 state) wsb
                     (dx-ext0 state) (- dx0 sq)
                     (dy-ext0 state) (- dy0 sq)
                     (dz-ext0 state) (- dz0 sq)
                     (dw-ext0 state) (- dw0 sq)
                     (dx-ext1 state) (- dx0 sq2)
                     (dy-ext1 state) (- dy0 sq2)
                     (dz-ext1 state) (- dz0 sq2)
                     (dw-ext1 state) (- dw0 sq2))
              (cond
                ((not (zerop (logand c1 1)))
                 (incf (xsv-ext0 state))
                 (decf (dx-ext0 state))
                 (incf (xsv-ext1 state) 2)
                 (decf (dx-ext1 state) 2))
                ((not (zerop (logand c1 2)))
                 (incf (ysv-ext0 state))
                 (decf (dy-ext0 state))
                 (incf (ysv-ext1 state) 2)
                 (decf (dy-ext1 state) 2))
                ((not (zerop (logand c1 4)))
                 (incf (zsv-ext0 state))
                 (decf (dz-ext0 state))
                 (incf (zsv-ext1 state) 2)
                 (decf (dz-ext1 state) 2))
                (t
                 (incf (wsv-ext0 state))
                 (decf (dw-ext0 state))
                 (incf (wsv-ext1 state) 2)
                 (decf (dw-ext1 state) 2)))
              (psetf (xsv-ext2 state) (1+ xsb)
                     (ysv-ext2 state) (1+ ysb)
                     (zsv-ext2 state) (1+ zsb)
                     (wsv-ext2 state) (1+ wsb)
                     (dx-ext2 state) (- dx0 1 sq2)
                     (dy-ext2 state) (- dy0 1 sq2)
                     (dz-ext2 state) (- dz0 1 sq2)
                     (dw-ext2 state) (- dw0 1 sq2))
              (cond
                ((zerop (logand c2 1))
                 (decf (xsv-ext2 state) 2)
                 (incf (dx-ext2 state) 2))
                ((zerop (logand c2 2))
                 (decf (ysv-ext2 state) 2)
                 (incf (dy-ext2 state) 2))
                ((zerop (logand c2 4))
                 (decf (zsv-ext2 state) 2)
                 (incf (dz-ext2 state) 2))
                (t
                 (decf (wsv-ext2 state) 2)
                 (incf (dw-ext2 state) 2))))
            (let ((c (logand point-a point-b)))
              (psetf (xsv-ext2 state) (1+ xsb)
                     (ysv-ext2 state) (1+ ysb)
                     (zsv-ext2 state) (1+ zsb)
                     (wsv-ext2 state) (1+ wsb)
                     (dx-ext2 state) (- dx0 1 sq4)
                     (dy-ext2 state) (- dy0 1 sq4)
                     (dz-ext2 state) (- dz0 1 sq4)
                     (dw-ext2 state) (- dw0 1 sq4))
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
                   ((zerop (logand c 1))
                    (incf (ysv-ext0 state))
                    (decf (dy-ext0 state)))
                   (t
                    (incf (ysv-ext1 state))
                    (decf (dy-ext1 state)))))
                (t
                 (let ((dy (- dy0 sq3)))
                   (psetf (ysv-ext0 state) ysb
                          (ysv-ext1 state) ysb
                          (dy-ext0 state) dy
                          (dy-ext1 state) dy))))
              (cond
                ((not (zerop (logand c 4)))
                 (let ((zsv (1+ zsb))
                       (dz (- dz0 1 sq3)))
                   (psetf (zsv-ext0 state) zsv
                          (zsv-ext1 state) zsv
                          (dz-ext0 state) dz
                          (dz-ext1 state) dz))
                 (cond
                   ((zerop (logand c 3))
                    (incf (zsv-ext0 state))
                    (decf (dz-ext0 state)))
                   (t
                    (incf (zsv-ext1 state))
                    (decf (dz-ext1 state)))))
                (t
                 (let ((dz (- dz0 sq3)))
                   (psetf (zsv-ext0 state) zsb
                          (zsv-ext1 state) zsb
                          (dz-ext0 state) dz
                          (dz-ext1 state) dz))))
              (cond
                ((not (zerop (logand c 8)))
                 (psetf (wsv-ext0 state) (1+ wsb)
                        (wsv-ext1 state) (+ wsb 2)
                        (dw-ext0 state) (- dw0 1 sq3)
                        (dw-ext1 state) (- dw0 2 sq3)))
                (t
                 (let ((dw (- dw0 sq3)))
                   (psetf (wsv-ext0 state) wsb
                          (wsv-ext1 state) wsb
                          (dw-ext0 state) dw
                          (dw-ext1 state) dw))))))
        (let ((c1 (if a-bigger-p point-a point-b))
              (c2 (if a-bigger-p point-b point-a)))
          (if (not (zerop (logand c1 1)))
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
            ((not (zerop (logand c1 2)))
             (let ((ysv (1+ ysb))
                   (dy (- dy0 1 sq3)))
               (psetf (ysv-ext0 state) ysv
                      (ysv-ext1 state) ysv
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))
             (cond
               ((zerop (logand c1 1))
                (incf (ysv-ext0 state))
                (decf (dy-ext0 state)))
               (t
                (incf (ysv-ext1 state))
                (decf (dy-ext1 state)))))
            (t
             (let ((dy (- dy0 sq3)))
               (psetf (ysv-ext0 state) ysb
                      (ysv-ext1 state) ysb
                      (dy-ext0 state) dy
                      (dy-ext1 state) dy))))
          (cond
            ((not (zerop (logand c1 4)))
             (let ((zsv (1+ zsb))
                   (dz (- dz0 1 sq3)))
               (psetf (zsv-ext0 state) zsv
                      (zsv-ext1 state) zsv
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz))
             (cond
               ((zerop (logand c1 3))
                (incf (zsv-ext0 state))
                (decf (dz-ext0 state)))
               (t
                (incf (zsv-ext1 state))
                (decf (dz-ext1 state)))))
            (t
             (let ((dz (- dz0 sq3)))
               (psetf (zsv-ext0 state) zsb
                      (zsv-ext1 state) zsb
                      (dz-ext0 state) dz
                      (dz-ext1 state) dz))))
          (if (not (zerop (logand c1 8)))
              (psetf (wsv-ext0 state) (1+ wsb)
                     (wsv-ext1 state) (+ wsb 2)
                     (dw-ext0 state) (- dw0 1 sq3)
                     (dw-ext1 state) (- dw0 2 sq3))
              (let ((dw (- dw0 sq3)))
                (psetf (wsv-ext0 state) wsb
                       (wsv-ext1 state) wsb
                       (dw-ext0 state) dw
                       (dw-ext1 state) dw)))
          (psetf (xsv-ext2 state) (1+ xsb)
                 (ysv-ext2 state) (1+ ysb)
                 (zsv-ext2 state) (1+ zsb)
                 (wsv-ext2 state) (1+ wsb)
                 (dx-ext2 state) (- dx0 1 sq2)
                 (dy-ext2 state) (- dy0 1 sq2)
                 (dz-ext2 state) (- dz0 1 sq2)
                 (dw-ext2 state) (- dw0 1 sq2))
          (cond
            ((zerop (logand c2 1))
             (decf (xsv-ext2 state) 2)
             (incf (dx-ext2 state) 2))
            ((zerop (logand c2 2))
             (decf (ysv-ext2 state) 2)
             (incf (dy-ext2 state) 2))
            ((zerop (logand c2 4))
             (decf (zsv-ext2 state) 2)
             (incf (dz-ext2 state) 2))
            (t
             (decf (wsv-ext2 state) 2)
             (incf (dw-ext2 state) 2)))))
    (let ((dw3 (- dw0 1 sq3))
          (dx4 (- dx0 1 sq3))
          (dy4 (- dy0 1 sq3))
          (dz4 (- dz0 1 sq3))
          (dx5 (- dx0 1 sq2))
          (dy5 (- dy0 1 sq2))
          (dz5 (- dz0 sq2))
          (dw5 (- dw0 sq2))
          (dy6 (- dy0 sq2))
          (dz6 (- dz0 1 sq2))
          (dw7 (- dw0 1 sq2))
          (dx8 (- dx0 sq2)))
      (psetf (dx4 state) dx4
             (dy4 state) dy4
             (dz4 state) dz4
             (dw4 state) (- dw0 sq3)
             (dx3 state) dx4
             (dy3 state) dy4
             (dz3 state) (- dz0 sq3)
             (dw3 state) dw3
             (dx2 state) dx4
             (dy2 state) (- dy0 sq3)
             (dz2 state) dz4
             (dw2 state) dw3
             (dx1 state) (- dx0 sq3)
             (dy1 state) dy4
             (dz1 state) dz4
             (dw1 state) dw3
             (dx5 state) dx5
             (dy5 state) dy5
             (dz5 state) dz5
             (dw5 state) dw5
             (dx6 state) dx5
             (dy6 state) dy6
             (dz6 state) dz6
             (dw6 state) dw5
             (dx7 state) dx5
             (dy7 state) dy6
             (dz7 state) dz5
             (dw7 state) dw7
             (dx8 state) dx8
             (dy8 state) dy5
             (dz8 state) dz6
             (dw8 state) dw5
             (dx9 state) dx8
             (dy9 state) dy5
             (dz9 state) dz5
             (dw9 state) dw7
             (dx10 state) dx8
             (dy10 state) dy6
             (dz10 state) dz6
             (dw10 state) dw7))
    (values)))

(defun sample (table x y z w)
  (declare (optimize speed)
           (double-float x y z w))
  (let ((state (make-state table x y z w)))
    (cond
      ((<= (ins state) 1)
       (in1 state)
       (contribute1 state))
      ((>= (ins state) 3)
       (in2 state)
       (contribute2 state))
      ((<= (ins state) 2)
       (in3 state)
       (contribute3 state))
      (t
       (in4 state)
       (contribute4 state)))
    (contribute5 state)
    (float (* (value state) +scale+) 1f0)))

(defmethod c::%make-sampler-func ((type (eql :open-simplex-4d)))
  (let ((table (rng:shuffle 'c::rng c:+perlin-permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (sample table x y z w))))
