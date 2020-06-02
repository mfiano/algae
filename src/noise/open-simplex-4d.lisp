(in-package #:net.mfiano.lisp.algae.noise)

(u:define-constant +open-simplex-4d/stretch+ (/ (1- (/ (sqrt 5d0))) 4))

(u:define-constant +open-simplex-4d/squish+ (/ (1- (sqrt 5d0)) 4))

(u:define-constant +open-simplex-4d/scale+ (/ 30d0))

(u:define-constant +open-simplex-4d/gradients+
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

(declaim (inline %make-open-simplex-4d-state))
(defstruct (open-simplex-4d-state
            (:constructor %make-open-simplex-4d-state)
            (:conc-name oss4-)
            (:predicate nil)
            (:copier nil))
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

(u:defun-inline make-open-simplex-4d-state (x y z w)
  (let* ((stretch-offset (* (+ x y z w) +open-simplex-4d/stretch+))
         (xs (+ x stretch-offset))
         (ys (+ y stretch-offset))
         (zs (+ z stretch-offset))
         (ws (+ w stretch-offset))
         (xsb (floor xs))
         (ysb (floor ys))
         (zsb (floor zs))
         (wsb (floor ws))
         (squish-offset (* (+ xsb ysb zsb wsb) +open-simplex-4d/squish+))
         (dx0 (- x (+ xsb squish-offset)))
         (dy0 (- y (+ ysb squish-offset)))
         (dz0 (- z (+ zsb squish-offset)))
         (dw0 (- w (+ wsb squish-offset)))
         (xins (- xs xsb))
         (yins (- ys ysb))
         (zins (- zs zsb))
         (wins (- ws wsb)))
    (declare (f50 xs ys zs ws))
    (%make-open-simplex-4d-state
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

(u:defun-inline open-simplex-4d/extrapolate (xsb ysb zsb wsb dx dy dz dw)
  (let ((index (logand (pget +permutation+ wsb zsb ysb xsb) 252)))
    (+ (* (aref +open-simplex-4d/gradients+ index) dx)
       (* (aref +open-simplex-4d/gradients+ (1+ index)) dy)
       (* (aref +open-simplex-4d/gradients+ (+ index 2)) dz)
       (* (aref +open-simplex-4d/gradients+ (+ index 3)) dw))))

(u:defun-inline open-simplex-4d/contribute (state dx dy dz dw xsb ysb zsb wsb)
  (let ((a (- 2 (* dx dx) (* dy dy) (* dz dz) (* dw dw))))
    (when (plusp a)
      (incf (oss4-value state)
            (* (expt a 4)
               (open-simplex-4d/extrapolate xsb ysb zsb wsb dx dy dz dw))))
    (values)))

(defun open-simplex-4d/contribute1 (state)
  (let ((xsb (oss4-xsb state))
        (ysb (oss4-ysb state))
        (zsb (oss4-zsb state))
        (wsb (oss4-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4-dx0 state)
                                (oss4-dy0 state)
                                (oss4-dz0 state)
                                (oss4-dw0 state)
                                xsb
                                ysb
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx1 state)
                                (oss4-dy1 state)
                                (oss4-dz1 state)
                                (oss4-dw1 state)
                                (1+ xsb)
                                ysb
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx2 state)
                                (oss4-dy2 state)
                                (oss4-dz2 state)
                                (oss4-dw2 state)
                                xsb
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx3 state)
                                (oss4-dy3 state)
                                (oss4-dz3 state)
                                (oss4-dw3 state)
                                xsb
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx4 state)
                                (oss4-dy4 state)
                                (oss4-dz4 state)
                                (oss4-dw4 state)
                                xsb
                                ysb
                                zsb
                                (1+ wsb))))

(defun open-simplex-4d/contribute2 (state)
  (let ((xsb (oss4-xsb state))
        (ysb (oss4-ysb state))
        (zsb (oss4-zsb state))
        (wsb (oss4-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4-dx4 state)
                                (oss4-dy4 state)
                                (oss4-dz4 state)
                                (oss4-dw4 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx3 state)
                                (oss4-dy3 state)
                                (oss4-dz3 state)
                                (oss4-dw3 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx2 state)
                                (oss4-dy2 state)
                                (oss4-dz2 state)
                                (oss4-dw2 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx1 state)
                                (oss4-dy1 state)
                                (oss4-dz1 state)
                                (oss4-dw1 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx0 state)
                                (oss4-dy0 state)
                                (oss4-dz0 state)
                                (oss4-dw0 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb)
                                (1+ wsb))))

(defun open-simplex-4d/contribute3 (state)
  (let ((xsb (oss4-xsb state))
        (ysb (oss4-ysb state))
        (zsb (oss4-zsb state))
        (wsb (oss4-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4-dx1 state)
                                (oss4-dy1 state)
                                (oss4-dz1 state)
                                (oss4-dw1 state)
                                (1+ xsb)
                                ysb
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx2 state)
                                (oss4-dy2 state)
                                (oss4-dz2 state)
                                (oss4-dw2 state)
                                xsb
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx3 state)
                                (oss4-dy3 state)
                                (oss4-dz3 state)
                                (oss4-dw3 state)
                                xsb
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx4 state)
                                (oss4-dy4 state)
                                (oss4-dz4 state)
                                (oss4-dw4 state)
                                xsb
                                ysb
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx5 state)
                                (oss4-dy5 state)
                                (oss4-dz5 state)
                                (oss4-dw5 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx6 state)
                                (oss4-dy6 state)
                                (oss4-dz6 state)
                                (oss4-dw6 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx7 state)
                                (oss4-dy7 state)
                                (oss4-dz7 state)
                                (oss4-dw7 state)
                                (1+ xsb)
                                ysb
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx8 state)
                                (oss4-dy8 state)
                                (oss4-dz8 state)
                                (oss4-dw8 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx9 state)
                                (oss4-dy9 state)
                                (oss4-dz9 state)
                                (oss4-dw9 state)
                                xsb
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx10 state)
                                (oss4-dy10 state)
                                (oss4-dz10 state)
                                (oss4-dw10 state)
                                xsb
                                ysb
                                (1+ zsb)
                                (1+ wsb))))

(defun open-simplex-4d/contribute4 (state)
  (let ((xsb (oss4-xsb state))
        (ysb (oss4-ysb state))
        (zsb (oss4-zsb state))
        (wsb (oss4-wsb state)))
    (open-simplex-4d/contribute state
                                (oss4-dx4 state)
                                (oss4-dy4 state)
                                (oss4-dz4 state)
                                (oss4-dw4 state)
                                (1+ xsb)
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx3 state)
                                (oss4-dy3 state)
                                (oss4-dz3 state)
                                (oss4-dw3 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx2 state)
                                (oss4-dy2 state)
                                (oss4-dz2 state)
                                (oss4-dw2 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx1 state)
                                (oss4-dy1 state)
                                (oss4-dz1 state)
                                (oss4-dw1 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx5 state)
                                (oss4-dy5 state)
                                (oss4-dz5 state)
                                (oss4-dw5 state)
                                (1+ xsb)
                                (1+ ysb)
                                zsb
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx6 state)
                                (oss4-dy6 state)
                                (oss4-dz6 state)
                                (oss4-dw6 state)
                                (1+ xsb)
                                ysb
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx7 state)
                                (oss4-dy7 state)
                                (oss4-dz7 state)
                                (oss4-dw7 state)
                                (1+ xsb)
                                ysb
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx8 state)
                                (oss4-dy8 state)
                                (oss4-dz8 state)
                                (oss4-dw8 state)
                                xsb
                                (1+ ysb)
                                (1+ zsb)
                                wsb)
    (open-simplex-4d/contribute state
                                (oss4-dx9 state)
                                (oss4-dy9 state)
                                (oss4-dz9 state)
                                (oss4-dw9 state)
                                xsb
                                (1+ ysb)
                                zsb
                                (1+ wsb))
    (open-simplex-4d/contribute state
                                (oss4-dx10 state)
                                (oss4-dy10 state)
                                (oss4-dz10 state)
                                (oss4-dw10 state)
                                xsb
                                ysb
                                (1+ zsb)
                                (1+ wsb))))

(defun open-simplex-4d/contribute5 (state)
  (open-simplex-4d/contribute state
                              (oss4-dx-ext0 state)
                              (oss4-dy-ext0 state)
                              (oss4-dz-ext0 state)
                              (oss4-dw-ext0 state)
                              (oss4-xsv-ext0 state)
                              (oss4-ysv-ext0 state)
                              (oss4-zsv-ext0 state)
                              (oss4-wsv-ext0 state))
  (open-simplex-4d/contribute state
                              (oss4-dx-ext1 state)
                              (oss4-dy-ext1 state)
                              (oss4-dz-ext1 state)
                              (oss4-dw-ext1 state)
                              (oss4-xsv-ext1 state)
                              (oss4-ysv-ext1 state)
                              (oss4-zsv-ext1 state)
                              (oss4-wsv-ext1 state))
  (open-simplex-4d/contribute state
                              (oss4-dx-ext2 state)
                              (oss4-dy-ext2 state)
                              (oss4-dz-ext2 state)
                              (oss4-dw-ext2 state)
                              (oss4-xsv-ext2 state)
                              (oss4-ysv-ext2 state)
                              (oss4-zsv-ext2 state)
                              (oss4-wsv-ext2 state)))

(defun open-simplex-4d/in1 (state)
  (let* ((point-a 1)
         (point-b 2)
         (score-a (oss4-xins state))
         (score-b (oss4-yins state))
         (zins (oss4-zins state))
         (wins (oss4-wins state))
         (uins (- 1 (oss4-ins state)))
         (xsb (oss4-xsb state))
         (ysb (oss4-ysb state))
         (zsb (oss4-zsb state))
         (wsb (oss4-wsb state))
         (dx0 (oss4-dx0 state))
         (dy0 (oss4-dy0 state))
         (dz0 (oss4-dz0 state))
         (dw0 (oss4-dw0 state))
         (sq +open-simplex-4d/squish+)
         (sq2 #.(* +open-simplex-4d/squish+ 2)))
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
              (psetf (oss4-xsv-ext0 state) (1- xsb)
                     (oss4-xsv-ext1 state) xsb
                     (oss4-xsv-ext2 state) xsb
                     (oss4-dx-ext0 state) (1+ dx0)
                     (oss4-dx-ext1 state) dx0
                     (oss4-dx-ext2 state) dx0)
              (psetf (oss4-xsv-ext0 state) (1+ xsb)
                     (oss4-xsv-ext1 state) (1+ xsb)
                     (oss4-xsv-ext2 state) (1+ xsb)
                     (oss4-dx-ext0 state) (1- dx0)
                     (oss4-dx-ext1 state) (1- dx0)
                     (oss4-dx-ext2 state) (1- dx0)))
          (cond
            ((zerop (logand c 2))
             (psetf (oss4-ysv-ext0 state) ysb
                    (oss4-ysv-ext1 state) ysb
                    (oss4-ysv-ext2 state) ysb
                    (oss4-dy-ext0 state) dy0
                    (oss4-dy-ext1 state) dy0
                    (oss4-dy-ext2 state) dy0)
             (cond
               ((= (logand c 1) 1)
                (decf (oss4-ysv-ext0 state))
                (incf (oss4-dy-ext0 state)))
               (t
                (decf (oss4-ysv-ext1 state))
                (incf (oss4-dy-ext1 state)))))
            (t
             (psetf (oss4-ysv-ext0 state) (1+ ysb)
                    (oss4-ysv-ext1 state) (1+ ysb)
                    (oss4-ysv-ext2 state) (1+ ysb)
                    (oss4-dy-ext0 state) (1- dy0)
                    (oss4-dy-ext1 state) (1- dy0)
                    (oss4-dy-ext2 state) (1- dy0))))
          (cond
            ((zerop (logand c 4))
             (psetf (oss4-zsv-ext0 state) zsb
                    (oss4-zsv-ext1 state) zsb
                    (oss4-zsv-ext2 state) zsb
                    (oss4-dz-ext0 state) dz0
                    (oss4-dz-ext1 state) dz0
                    (oss4-dz-ext2 state) dz0)
             (cond
               ((not (zerop (logand c 3)))
                ;; note, this UNLESS was two branches in the reference
                ;; implementation, but it was wrong, and SBCL correctly detected
                ;; that it could remove dead code in the first branch.
                (unless (= (logand c 3) 3)
                  (decf (oss4-zsv-ext1 state))
                  (incf (oss4-dz-ext1 state))))
               (t
                (decf (oss4-zsv-ext2 state))
                (incf (oss4-dz-ext2 state)))))
            (t
             (psetf (oss4-zsv-ext0 state) (1+ zsb)
                    (oss4-zsv-ext1 state) (1+ zsb)
                    (oss4-zsv-ext2 state) (1+ zsb)
                    (oss4-dz-ext0 state) (1- dz0)
                    (oss4-dz-ext1 state) (1- dz0)
                    (oss4-dz-ext2 state) (1- dz0))))
          (if (zerop (logand c 8))
              (psetf (oss4-wsv-ext0 state) wsb
                     (oss4-wsv-ext1 state) wsb
                     (oss4-wsv-ext2 state) (1- wsb)
                     (oss4-dw-ext0 state) dw0
                     (oss4-dw-ext1 state) dw0
                     (oss4-dw-ext2 state) (1+ dw0))
              (psetf (oss4-wsv-ext0 state) (1+ wsb)
                     (oss4-wsv-ext1 state) (1+ wsb)
                     (oss4-wsv-ext2 state) (1+ wsb)
                     (oss4-dw-ext0 state) (1- dw0)
                     (oss4-dw-ext1 state) (1- dw0)
                     (oss4-dw-ext2 state) (1- dw0))))
        (let ((c (logior point-a point-b)))
          (if (zerop (logand c 1))
              (psetf (oss4-xsv-ext0 state) xsb
                     (oss4-xsv-ext1 state) (1- xsb)
                     (oss4-xsv-ext2 state) xsb
                     (oss4-dx-ext0 state) (- dx0 sq2)
                     (oss4-dx-ext1 state) (- (1+ dx0) sq)
                     (oss4-dx-ext2 state) (- dx0 sq))
              (psetf (oss4-xsv-ext0 state) (1+ xsb)
                     (oss4-xsv-ext1 state) (1+ xsb)
                     (oss4-xsv-ext2 state) (1+ xsb)
                     (oss4-dx-ext0 state) (- dx0 1 sq2)
                     (oss4-dx-ext1 state) (- dx0 1 sq)
                     (oss4-dx-ext2 state) (- dx0 1 sq)))
          (cond
            ((zerop (logand c 2))
             (psetf (oss4-ysv-ext0 state) ysb
                    (oss4-ysv-ext1 state) ysb
                    (oss4-ysv-ext2 state) ysb
                    (oss4-dy-ext0 state) (- dy0 sq2)
                    (oss4-dy-ext1 state) (- dy0 sq)
                    (oss4-dy-ext2 state) (- dy0 sq))
             (cond
               ((= (logand c 1) 1)
                (decf (oss4-ysv-ext1 state))
                (incf (oss4-dy-ext1 state)))
               (t
                (decf (oss4-ysv-ext2 state))
                (incf (oss4-dy-ext2 state)))))
            (t
             (psetf (oss4-ysv-ext0 state) (1+ ysb)
                    (oss4-ysv-ext1 state) (1+ ysb)
                    (oss4-ysv-ext2 state) (1+ ysb)
                    (oss4-dy-ext0 state) (- dy0 1 sq2)
                    (oss4-dy-ext1 state) (- dy0 1 sq)
                    (oss4-dy-ext2 state) (- dy0 1 sq))))
          (cond
            ((zerop (logand c 4))
             (psetf (oss4-zsv-ext0 state) zsb
                    (oss4-zsv-ext1 state) zsb
                    (oss4-zsv-ext2 state) zsb
                    (oss4-dz-ext0 state) (- dz0 sq2)
                    (oss4-dz-ext1 state) (- dz0 sq)
                    (oss4-dz-ext2 state) (- dz0 sq))
             (cond
               ((= (logand c 3) 3)
                (decf (oss4-zsv-ext1 state))
                (incf (oss4-dz-ext1 state)))
               (t
                (decf (oss4-zsv-ext2 state))
                (incf (oss4-dz-ext2 state)))))
            (t
             (psetf (oss4-zsv-ext0 state) (1+ zsb)
                    (oss4-zsv-ext1 state) (1+ zsb)
                    (oss4-zsv-ext2 state) (1+ zsb)
                    (oss4-dz-ext0 state) (- dz0 1 sq2)
                    (oss4-dz-ext1 state) (- dz0 1 sq)
                    (oss4-dz-ext2 state) (- dz0 1 sq))))
          (cond
            ((zerop (logand c 8))
             (psetf (oss4-wsv-ext0 state) wsb
                    (oss4-wsv-ext1 state) wsb
                    (oss4-wsv-ext2 state) (1- wsb)
                    (oss4-dw-ext0 state) (- dw0 sq2)
                    (oss4-dw-ext1 state) (- dw0 sq)
                    (oss4-dw-ext2 state) (- (1+ dw0) sq)))
            (t
             (psetf (oss4-wsv-ext0 state) (1+ wsb)
                    (oss4-wsv-ext1 state) (1+ wsb)
                    (oss4-wsv-ext2 state) (1+ wsb)
                    (oss4-dw-ext0 state) (- dw0 1 sq2)
                    (oss4-dw-ext1 state) (- dw0 1 sq)
                    (oss4-dw-ext2 state) (- dw0 1 sq))))))
    (setf (oss4-dx1 state) (- dx0 1 sq)
          (oss4-dy1 state) (- dy0 sq)
          (oss4-dz1 state) (- dz0 sq)
          (oss4-dw1 state) (- dw0 sq)
          (oss4-dx2 state) (- dx0 sq)
          (oss4-dy2 state) (- dy0 1 sq)
          (oss4-dz2 state) (oss4-dz1 state)
          (oss4-dw2 state) (oss4-dw1 state)
          (oss4-dx3 state) (oss4-dx2 state)
          (oss4-dy3 state) (oss4-dy1 state)
          (oss4-dz3 state) (- dz0 1 sq)
          (oss4-dw3 state) (oss4-dw1 state)
          (oss4-dx4 state) (oss4-dx2 state)
          (oss4-dy4 state) (oss4-dy1 state)
          (oss4-dz4 state) (oss4-dz1 state)
          (oss4-dw4 state) (- dw0 1 sq))
    (values)))

(defun open-simplex-4d/in2 (state)
  (let* ((point-a 14)
         (point-b 13)
         (score-a (oss4-xins state))
         (score-b (oss4-yins state))
         (zins (oss4-zins state))
         (wins (oss4-wins state))
         (uins (- 4 (oss4-ins state)))
         (xsb (oss4-xsb state))
         (ysb (oss4-ysb state))
         (zsb (oss4-zsb state))
         (wsb (oss4-wsb state))
         (dx0 (oss4-dx0 state))
         (dy0 (oss4-dy0 state))
         (dz0 (oss4-dz0 state))
         (dw0 (oss4-dw0 state))
         (sq2 #.(* +open-simplex-4d/squish+ 2))
         (sq3 #.(* +open-simplex-4d/squish+ 3))
         (sq4 #.(* +open-simplex-4d/squish+ 4)))
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
              (psetf (oss4-xsv-ext0 state) (+ xsb 2)
                     (oss4-xsv-ext1 state) (1+ xsb)
                     (oss4-xsv-ext2 state) (1+ xsb)
                     (oss4-dx-ext0 state) (- dx0 2 sq4)
                     (oss4-dx-ext1 state) (- dx0 1 sq4)
                     (oss4-dx-ext2 state) (- dx0 1 sq4))
              (psetf (oss4-xsv-ext0 state) xsb
                     (oss4-xsv-ext1 state) xsb
                     (oss4-xsv-ext2 state) xsb
                     (oss4-dx-ext0 state) (- dx0 sq4)
                     (oss4-dx-ext1 state) (- dx0 sq4)
                     (oss4-dx-ext2 state) (- dx0 sq4)))
          (cond
            ((not (zerop (logand c 2)))
             (psetf (oss4-ysv-ext0 state) (1+ ysb)
                    (oss4-ysv-ext1 state) (1+ ysb)
                    (oss4-ysv-ext2 state) (1+ ysb)
                    (oss4-dy-ext0 state) (- dy0 1 sq4)
                    (oss4-dy-ext1 state) (- dy0 1 sq4)
                    (oss4-dy-ext2 state) (- dy0 1 sq4))
             (cond
               ((not (zerop (logand c 1)))
                (incf (oss4-ysv-ext1 state))
                (decf (oss4-dy-ext1 state)))
               (t
                (incf (oss4-ysv-ext0 state))
                (decf (oss4-dy-ext0 state)))))
            (t
             (psetf (oss4-ysv-ext0 state) ysb
                    (oss4-ysv-ext1 state) ysb
                    (oss4-ysv-ext2 state) ysb
                    (oss4-dy-ext0 state) (- dy0 sq4)
                    (oss4-dy-ext1 state) (- dy0 sq4)
                    (oss4-dy-ext2 state) (- dy0 sq4))))
          (cond
            ((not (zerop (logand c 4)))
             (psetf (oss4-zsv-ext0 state) (1+ zsb)
                    (oss4-zsv-ext1 state) (1+ zsb)
                    (oss4-zsv-ext2 state) (1+ zsb)
                    (oss4-dz-ext0 state) (- dz0 1 sq4)
                    (oss4-dz-ext1 state) (- dz0 1 sq4)
                    (oss4-dz-ext2 state) (- dz0 1 sq4))
             (cond
               ((not (= (logand c 3) 3))
                ;; note, this UNLESS was two branches in the reference
                ;; implementation, but it was wrong, and SBCL correctly detected
                ;; that it could remove dead code in the first branch.
                (unless (zerop (logand c 3))
                  (incf (oss4-zsv-ext1 state))
                  (decf (oss4-dz-ext1 state))))
               (t
                (incf (oss4-zsv-ext2 state))
                (decf (oss4-dz-ext2 state)))))
            (t
             (psetf (oss4-zsv-ext0 state) zsb
                    (oss4-zsv-ext1 state) zsb
                    (oss4-zsv-ext2 state) zsb
                    (oss4-dz-ext0 state) (- dz0 sq4)
                    (oss4-dz-ext1 state) (- dz0 sq4)
                    (oss4-dz-ext2 state) (- dz0 sq4))))
          (if (not (zerop (logand c 8)))
              (psetf (oss4-wsv-ext0 state) (1+ wsb)
                     (oss4-wsv-ext1 state) (1+ wsb)
                     (oss4-wsv-ext2 state) (+ wsb 2)
                     (oss4-dw-ext0 state) (- dw0 1 sq4)
                     (oss4-dw-ext1 state) (- dw0 1 sq4)
                     (oss4-dw-ext2 state) (- dw0 2 sq4))
              (psetf (oss4-wsv-ext0 state) wsb
                     (oss4-wsv-ext1 state) wsb
                     (oss4-wsv-ext2 state) wsb
                     (oss4-dw-ext0 state) (- dw0 sq4)
                     (oss4-dw-ext1 state) (- dw0 sq4)
                     (oss4-dw-ext2 state) (- dw0 sq4))))
        (let ((c (logand point-a point-b)))
          (if (not (zerop (logand c 1)))
              (psetf (oss4-xsv-ext0 state) (1+ xsb)
                     (oss4-xsv-ext1 state) (+ xsb 2)
                     (oss4-xsv-ext2 state) (1+ xsb)
                     (oss4-dx-ext0 state) (- dx0 1 sq2)
                     (oss4-dx-ext1 state) (- dx0 2 sq3)
                     (oss4-dx-ext2 state) (- dx0 1 sq3))
              (psetf (oss4-xsv-ext0 state) xsb
                     (oss4-xsv-ext1 state) xsb
                     (oss4-xsv-ext2 state) xsb
                     (oss4-dx-ext0 state) (- dx0 sq2)
                     (oss4-dx-ext1 state) (- dx0 sq3)
                     (oss4-dx-ext2 state) (- dx0 sq3)))
          (cond
            ((not (zerop (logand c 2)))
             (psetf (oss4-ysv-ext0 state) (1+ ysb)
                    (oss4-ysv-ext1 state) (1+ ysb)
                    (oss4-ysv-ext2 state) (1+ ysb)
                    (oss4-dy-ext0 state) (- dy0 1 sq2)
                    (oss4-dy-ext1 state) (- dy0 1 sq3)
                    (oss4-dy-ext2 state) (- dy0 1 sq3))
             (cond
               ((not (zerop (logand c 1)))
                (incf (oss4-ysv-ext2 state))
                (decf (oss4-dy-ext2 state)))
               (t
                (incf (oss4-ysv-ext1 state))
                (decf (oss4-dy-ext1 state)))))
            (t
             (psetf (oss4-ysv-ext0 state) ysb
                    (oss4-ysv-ext1 state) ysb
                    (oss4-ysv-ext2 state) ysb
                    (oss4-dy-ext0 state) (- dy0 sq2)
                    (oss4-dy-ext1 state) (- dy0 sq3)
                    (oss4-dy-ext2 state) (- dy0 sq3))))
          (cond
            ((not (zerop (logand c 4)))
             (psetf (oss4-zsv-ext0 state) (1+ zsb)
                    (oss4-zsv-ext1 state) (1+ zsb)
                    (oss4-zsv-ext2 state) (1+ zsb)
                    (oss4-dz-ext0 state) (- dz0 1 sq2)
                    (oss4-dz-ext1 state) (- dz0 1 sq3)
                    (oss4-dz-ext2 state) (- dz0 1 sq3))
             (cond
               ((not (zerop (logand c 3)))
                (incf (oss4-zsv-ext2 state))
                (decf (oss4-dz-ext2 state)))
               (t
                (incf (oss4-zsv-ext1 state))
                (decf (oss4-dz-ext1 state)))))
            (t
             (psetf (oss4-zsv-ext0 state) zsb
                    (oss4-zsv-ext1 state) zsb
                    (oss4-zsv-ext2 state) zsb
                    (oss4-dz-ext0 state) (- dz0 sq2)
                    (oss4-dz-ext1 state) (- dz0 sq3)
                    (oss4-dz-ext2 state) (- dz0 sq3))))
          (cond
            ((not (zerop (logand c 8)))
             (psetf (oss4-wsv-ext0 state) (1+ wsb)
                    (oss4-wsv-ext1 state) (1+ wsb)
                    (oss4-wsv-ext2 state) (+ wsb 2)
                    (oss4-dw-ext0 state) (- dw0 1 sq2)
                    (oss4-dw-ext1 state) (- dw0 1 sq3)
                    (oss4-dw-ext2 state) (- dw0 2 sq3)))
            (t
             (psetf (oss4-wsv-ext0 state) wsb
                    (oss4-wsv-ext1 state) wsb
                    (oss4-wsv-ext2 state) wsb
                    (oss4-dw-ext0 state) (- dw0 sq2)
                    (oss4-dw-ext1 state) (- dw0 sq3)
                    (oss4-dw-ext2 state) (- dw0 sq3))))))
    (setf (oss4-dx4 state) (- dx0 1 sq3)
          (oss4-dy4 state) (- dy0 1 sq3)
          (oss4-dz4 state) (- dz0 1 sq3)
          (oss4-dw4 state) (- dw0 sq3)
          (oss4-dx3 state) (oss4-dx4 state)
          (oss4-dy3 state) (oss4-dy4 state)
          (oss4-dz3 state) (- dz0 sq3)
          (oss4-dw3 state) (- dw0 1 sq3)
          (oss4-dx2 state) (oss4-dx4 state)
          (oss4-dy2 state) (- dy0 sq3)
          (oss4-dz2 state) (oss4-dz4 state)
          (oss4-dw2 state) (oss4-dw3 state)
          (oss4-dx1 state) (- dx0 sq3)
          (oss4-dy1 state) (oss4-dy4 state)
          (oss4-dz1 state) (oss4-dz4 state)
          (oss4-dw1 state) (oss4-dw3 state)
          (oss4-dx0 state) (- dx0 1 sq4)
          (oss4-dy0 state) (- dy0 1 sq4)
          (oss4-dz0 state) (- dz0 1 sq4)
          (oss4-dw0 state) (- dw0 1 sq4))
    (values)))

(defun open-simplex-4d/in3 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-bigger-p t)
         (b-bigger-p t)
         (xins (oss4-xins state))
         (yins (oss4-yins state))
         (zins (oss4-zins state))
         (wins (oss4-wins state))
         (ins (oss4-ins state))
         (xsb (oss4-xsb state))
         (ysb (oss4-ysb state))
         (zsb (oss4-zsb state))
         (wsb (oss4-wsb state))
         (dx0 (oss4-dx0 state))
         (dy0 (oss4-dy0 state))
         (dz0 (oss4-dz0 state))
         (dw0 (oss4-dw0 state))
         (sq +open-simplex-4d/squish+)
         (sq2 #.(* +open-simplex-4d/squish+ 2))
         (sq3 #.(* +open-simplex-4d/squish+ 3)))
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
                  (psetf (oss4-xsv-ext0 state) xsb
                         (oss4-xsv-ext1 state) (1- xsb)
                         (oss4-dx-ext0 state) (- dx0 sq3)
                         (oss4-dx-ext1 state) (- (1+ dx0) sq2))
                  (psetf (oss4-xsv-ext0 state) (1+ xsb)
                         (oss4-xsv-ext1 state) (1+ xsb)
                         (oss4-dx-ext0 state) (- dx0 1 sq3)
                         (oss4-dx-ext1 state) (- dx0 1 sq2)))
              (if (zerop (logand c1 2))
                  (psetf (oss4-ysv-ext0 state) ysb
                         (oss4-ysv-ext1 state) (1- ysb)
                         (oss4-dy-ext0 state) (- dy0 sq3)
                         (oss4-dy-ext1 state) (- (1+ dy0) sq2))
                  (psetf (oss4-ysv-ext0 state) (1+ ysb)
                         (oss4-ysv-ext1 state) (1+ ysb)
                         (oss4-dy-ext0 state) (- dy0 1 sq3)
                         (oss4-dy-ext1 state) (- dy0 1 sq2)))
              (if (zerop (logand c1 4))
                  (psetf (oss4-zsv-ext0 state) zsb
                         (oss4-zsv-ext1 state) (1- zsb)
                         (oss4-dz-ext0 state) (- dz0 sq3)
                         (oss4-dz-ext1 state) (- (1+ dz0) sq2))
                  (psetf (oss4-zsv-ext0 state) (1+ zsb)
                         (oss4-zsv-ext1 state) (1+ zsb)
                         (oss4-dz-ext0 state) (- dz0 1 sq3)
                         (oss4-dz-ext1 state) (- dz0 1 sq2)))
              (if (zerop (logand c1 8))
                  (psetf (oss4-wsv-ext0 state) wsb
                         (oss4-wsv-ext1 state) (1- wsb)
                         (oss4-dw-ext0 state) (- dw0 sq3)
                         (oss4-dw-ext1 state) (- (1+ dw0) sq2))
                  (psetf (oss4-wsv-ext0 state) (1+ wsb)
                         (oss4-wsv-ext1 state) (1+ wsb)
                         (oss4-dw-ext0 state) (- dw0 1 sq3)
                         (oss4-dw-ext1 state) (- dw0 1 sq2)))
              (psetf (oss4-xsv-ext2 state) xsb
                     (oss4-ysv-ext2 state) ysb
                     (oss4-zsv-ext2 state) zsb
                     (oss4-wsv-ext2 state) wsb
                     (oss4-dx-ext2 state) (- dx0 sq2)
                     (oss4-dy-ext2 state) (- dy0 sq2)
                     (oss4-dz-ext2 state) (- dz0 sq2)
                     (oss4-dw-ext2 state) (- dw0 sq2))
              (cond
                ((not (zerop (logand c2 1)))
                 (incf (oss4-xsv-ext2 state) 2)
                 (decf (oss4-dx-ext2 state) 2))
                ((not (zerop (logand c2 2)))
                 (incf (oss4-ysv-ext2 state) 2)
                 (decf (oss4-dy-ext2 state) 2))
                ((not (zerop (logand c2 4)))
                 (incf (oss4-zsv-ext2 state) 2)
                 (decf (oss4-dz-ext2 state) 2))
                (t
                 (incf (oss4-wsv-ext2 state) 2)
                 (decf (oss4-dw-ext2 state) 2))))
            (let ((c (logior point-a point-b)))
              (psetf (oss4-xsv-ext2 state) xsb
                     (oss4-ysv-ext2 state) ysb
                     (oss4-zsv-ext2 state) zsb
                     (oss4-wsv-ext2 state) wsb
                     (oss4-dx-ext2 state) dx0
                     (oss4-dy-ext2 state) dy0
                     (oss4-dz-ext2 state) dz0
                     (oss4-dw-ext2 state) dw0)
              (if (zerop (logand c 1))
                  (psetf (oss4-xsv-ext0 state) (1- xsb)
                         (oss4-xsv-ext1 state) xsb
                         (oss4-dx-ext0 state) (- (1+ dx0) sq)
                         (oss4-dx-ext1 state) (- dx0 sq))
                  (psetf (oss4-xsv-ext0 state) (1+ xsb)
                         (oss4-xsv-ext1 state) (1+ xsb)
                         (oss4-dx-ext0 state) (- dx0 1 sq)
                         (oss4-dx-ext1 state) (- dx0 1 sq)))
              (cond
                ((zerop (logand c 2))
                 (psetf (oss4-ysv-ext0 state) ysb
                        (oss4-ysv-ext1 state) ysb
                        (oss4-dy-ext0 state) (- dy0 sq)
                        (oss4-dy-ext1 state) (- dy0 sq))
                 (cond
                   ((= (logand c 1) 1)
                    (decf (oss4-ysv-ext0 state))
                    (incf (oss4-dy-ext0 state)))
                   (t
                    (decf (oss4-ysv-ext1 state))
                    (incf (oss4-dy-ext1 state)))))
                (t
                 (psetf (oss4-ysv-ext0 state) (1+ ysb)
                        (oss4-ysv-ext1 state) (1+ ysb)
                        (oss4-dy-ext0 state) (- dy0 1 sq)
                        (oss4-dy-ext1 state) (- dy0 1 sq))))
              (cond
                ((zerop (logand c 4))
                 (psetf (oss4-zsv-ext0 state) zsb
                        (oss4-zsv-ext1 state) zsb
                        (oss4-dz-ext0 state) (- dz0 sq)
                        (oss4-dz-ext1 state) (- dz0 sq))
                 (cond
                   ((= (logand c 3) 3)
                    (decf (oss4-zsv-ext0 state))
                    (incf (oss4-dz-ext0 state)))
                   (t
                    (decf (oss4-zsv-ext1 state))
                    (incf (oss4-dz-ext1 state)))))
                (t
                 (psetf (oss4-zsv-ext0 state) (1+ zsb)
                        (oss4-zsv-ext1 state) (1+ zsb)
                        (oss4-dz-ext0 state) (- dz0 1 sq)
                        (oss4-dz-ext1 state) (- dz0 1 sq))))
              (cond
                ((zerop (logand c 8))
                 (psetf (oss4-wsv-ext0 state) wsb
                        (oss4-wsv-ext1 state) (1- wsb)
                        (oss4-dw-ext0 state) (- dw0 sq)
                        (oss4-dw-ext1 state) (- (1+ dw0) sq)))
                (t
                 (psetf (oss4-wsv-ext0 state) (1+ wsb)
                        (oss4-wsv-ext1 state) (1+ wsb)
                        (oss4-dw-ext0 state) (- dw0 1 sq)
                        (oss4-dw-ext1 state) (- dw0 1 sq))))))
        (let ((c1 (if a-bigger-p point-a point-b))
              (c2 (if a-bigger-p point-b point-a)))
          (if (zerop (logand c1 1))
              (psetf (oss4-xsv-ext0 state) (1- xsb)
                     (oss4-xsv-ext1 state) xsb
                     (oss4-dx-ext0 state) (- (1+ dx0) sq)
                     (oss4-dx-ext1 state) (- dx0 sq))
              (psetf (oss4-xsv-ext0 state) (1+ xsb)
                     (oss4-xsv-ext1 state) (1+ xsb)
                     (oss4-dx-ext0 state) (- dx0 1 sq)
                     (oss4-dx-ext1 state) (- dx0 1 sq)))
          (cond
            ((zerop (logand c1 2))
             (psetf (oss4-ysv-ext0 state) ysb
                    (oss4-ysv-ext1 state) ysb
                    (oss4-dy-ext0 state) (- dy0 sq)
                    (oss4-dy-ext1 state) (- dy0 sq))
             (cond
               ((= (logand c1 1) 1)
                (decf (oss4-ysv-ext0 state))
                (incf (oss4-dy-ext0 state)))
               (t
                (decf (oss4-ysv-ext1 state))
                (incf (oss4-dy-ext1 state)))))
            (t
             (psetf (oss4-ysv-ext0 state) (1+ ysb)
                    (oss4-ysv-ext1 state) (1+ ysb)
                    (oss4-dy-ext0 state) (- dy0 1 sq)
                    (oss4-dy-ext1 state) (- dy0 1 sq))))
          (cond
            ((zerop (logand c1 4))
             (psetf (oss4-zsv-ext0 state) zsb
                    (oss4-zsv-ext1 state) zsb
                    (oss4-dz-ext0 state) (- dz0 sq)
                    (oss4-dz-ext1 state) (- dz0 sq))
             (cond
               ((= (logand c1 3) 3)
                (decf (oss4-zsv-ext0 state))
                (incf (oss4-dz-ext0 state)))
               (t
                (decf (oss4-zsv-ext1 state))
                (incf (oss4-dz-ext1 state)))))
            (t
             (psetf (oss4-zsv-ext0 state) (1+ zsb)
                    (oss4-zsv-ext1 state) (1+ zsb)
                    (oss4-dz-ext0 state) (- dz0 1 sq)
                    (oss4-dz-ext1 state) (- dz0 1 sq))))
          (if (zerop (logand c1 8))
              (psetf (oss4-wsv-ext0 state) wsb
                     (oss4-wsv-ext1 state) (1- wsb)
                     (oss4-dw-ext0 state) (- dw0 sq)
                     (oss4-dw-ext1 state) (- (1+ dw0) sq))
              (psetf (oss4-wsv-ext0 state) (1+ wsb)
                     (oss4-wsv-ext1 state) (1+ wsb)
                     (oss4-dw-ext0 state) (- dw0 1 sq)
                     (oss4-dw-ext1 state) (- dw0 1 sq)))
          (psetf (oss4-xsv-ext2 state) xsb
                 (oss4-ysv-ext2 state) ysb
                 (oss4-zsv-ext2 state) zsb
                 (oss4-wsv-ext2 state) wsb
                 (oss4-dx-ext2 state) (- dx0 sq2)
                 (oss4-dy-ext2 state) (- dy0 sq2)
                 (oss4-dz-ext2 state) (- dz0 sq2)
                 (oss4-dw-ext2 state) (- dw0 sq2))
          (cond
            ((not (zerop (logand c2 1)))
             (incf (oss4-xsv-ext2 state) 2)
             (decf (oss4-dx-ext2 state) 2))
            ((not (zerop (logand c2 2)))
             (incf (oss4-ysv-ext2 state) 2)
             (decf (oss4-dy-ext2 state) 2))
            ((not (zerop (logand c2 4)))
             (incf (oss4-zsv-ext2 state) 2)
             (decf (oss4-dz-ext2 state) 2))
            (t
             (incf (oss4-wsv-ext2 state) 2)
             (decf (oss4-dw-ext2 state) 2)))))
    (setf (oss4-dx1 state) (- dx0 1 sq)
          (oss4-dy1 state) (- dy0 sq)
          (oss4-dz1 state) (- dz0 sq)
          (oss4-dw1 state) (- dw0 sq)
          (oss4-dx2 state) (- dx0 sq)
          (oss4-dy2 state) (- dy0 1 sq)
          (oss4-dz2 state) (oss4-dz1 state)
          (oss4-dw2 state) (oss4-dw1 state)
          (oss4-dx3 state) (oss4-dx2 state)
          (oss4-dy3 state) (oss4-dy1 state)
          (oss4-dz3 state) (- dz0 1 sq)
          (oss4-dw3 state) (oss4-dw1 state)
          (oss4-dx4 state) (oss4-dx2 state)
          (oss4-dy4 state) (oss4-dy1 state)
          (oss4-dz4 state) (oss4-dz1 state)
          (oss4-dw4 state) (- dw0 1 sq)
          (oss4-dx5 state) (- dx0 1 sq2)
          (oss4-dy5 state) (- dy0 1 sq2)
          (oss4-dz5 state) (- dz0 sq2)
          (oss4-dw5 state) (- dw0 sq2)
          (oss4-dx6 state) (- dx0 1 sq2)
          (oss4-dy6 state) (- dy0 sq2)
          (oss4-dz6 state) (- dz0 1 sq2)
          (oss4-dw6 state) (- dw0 sq2)
          (oss4-dx7 state) (- dx0 1 sq2)
          (oss4-dy7 state) (- dy0 sq2)
          (oss4-dz7 state) (- dz0 sq2)
          (oss4-dw7 state) (- dw0 1 sq2)
          (oss4-dx8 state) (- dx0 sq2)
          (oss4-dy8 state) (- dy0 1 sq2)
          (oss4-dz8 state) (- dz0 1 sq2)
          (oss4-dw8 state) (- dw0 sq2)
          (oss4-dx9 state) (- dx0 sq2)
          (oss4-dy9 state) (- dy0 1 sq2)
          (oss4-dz9 state) (- dz0 sq2)
          (oss4-dw9 state) (- dw0 1 sq2)
          (oss4-dx10 state) (- dx0 sq2)
          (oss4-dy10 state) (- dy0 sq2)
          (oss4-dz10 state) (- dz0 1 sq2)
          (oss4-dw10 state) (- dw0 1 sq2))
    (values)))

(defun open-simplex-4d/in4 (state)
  (let* ((point-a 0)
         (point-b 0)
         (score-a 0d0)
         (score-b 0d0)
         (a-bigger-p t)
         (b-bigger-p t)
         (xins (oss4-xins state))
         (yins (oss4-yins state))
         (zins (oss4-zins state))
         (wins (oss4-wins state))
         (ins (oss4-ins state))
         (xsb (oss4-xsb state))
         (ysb (oss4-ysb state))
         (zsb (oss4-zsb state))
         (wsb (oss4-wsb state))
         (dx0 (oss4-dx0 state))
         (dy0 (oss4-dy0 state))
         (dz0 (oss4-dz0 state))
         (dw0 (oss4-dw0 state))
         (sq +open-simplex-4d/squish+)
         (sq2 #.(* +open-simplex-4d/squish+ 2))
         (sq3 #.(* +open-simplex-4d/squish+ 3))
         (sq4 #.(* +open-simplex-4d/squish+ 4)))
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
              (psetf (oss4-xsv-ext0 state) xsb
                     (oss4-xsv-ext1 state) xsb
                     (oss4-ysv-ext0 state) ysb
                     (oss4-ysv-ext1 state) ysb
                     (oss4-zsv-ext0 state) zsb
                     (oss4-zsv-ext1 state) zsb
                     (oss4-wsv-ext0 state) wsb
                     (oss4-wsv-ext1 state) wsb
                     (oss4-dx-ext0 state) (- dx0 sq)
                     (oss4-dy-ext0 state) (- dy0 sq)
                     (oss4-dz-ext0 state) (- dz0 sq)
                     (oss4-dw-ext0 state) (- dw0 sq)
                     (oss4-dx-ext1 state) (- dx0 sq2)
                     (oss4-dy-ext1 state) (- dy0 sq2)
                     (oss4-dz-ext1 state) (- dz0 sq2)
                     (oss4-dw-ext1 state) (- dw0 sq2))
              (cond
                ((not (zerop (logand c1 1)))
                 (incf (oss4-xsv-ext0 state))
                 (decf (oss4-dx-ext0 state))
                 (incf (oss4-xsv-ext1 state) 2)
                 (decf (oss4-dx-ext1 state) 2))
                ((not (zerop (logand c1 2)))
                 (incf (oss4-ysv-ext0 state))
                 (decf (oss4-dy-ext0 state))
                 (incf (oss4-ysv-ext1 state) 2)
                 (decf (oss4-dy-ext1 state) 2))
                ((not (zerop (logand c1 4)))
                 (incf (oss4-zsv-ext0 state))
                 (decf (oss4-dz-ext0 state))
                 (incf (oss4-zsv-ext1 state) 2)
                 (decf (oss4-dz-ext1 state) 2))
                (t
                 (incf (oss4-wsv-ext0 state))
                 (decf (oss4-dw-ext0 state))
                 (incf (oss4-wsv-ext1 state) 2)
                 (decf (oss4-dw-ext1 state) 2)))
              (psetf (oss4-xsv-ext2 state) (1+ xsb)
                     (oss4-ysv-ext2 state) (1+ ysb)
                     (oss4-zsv-ext2 state) (1+ zsb)
                     (oss4-wsv-ext2 state) (1+ wsb)
                     (oss4-dx-ext2 state) (- dx0 1 sq2)
                     (oss4-dy-ext2 state) (- dy0 1 sq2)
                     (oss4-dz-ext2 state) (- dz0 1 sq2)
                     (oss4-dw-ext2 state) (- dw0 1 sq2))
              (cond
                ((zerop (logand c2 1))
                 (decf (oss4-xsv-ext2 state) 2)
                 (incf (oss4-dx-ext2 state) 2))
                ((zerop (logand c2 2))
                 (decf (oss4-ysv-ext2 state) 2)
                 (incf (oss4-dy-ext2 state) 2))
                ((zerop (logand c2 4))
                 (decf (oss4-zsv-ext2 state) 2)
                 (incf (oss4-dz-ext2 state) 2))
                (t
                 (decf (oss4-wsv-ext2 state) 2)
                 (incf (oss4-dw-ext2 state) 2))))
            (let ((c (logand point-a point-b)))
              (psetf (oss4-xsv-ext2 state) (1+ xsb)
                     (oss4-ysv-ext2 state) (1+ ysb)
                     (oss4-zsv-ext2 state) (1+ zsb)
                     (oss4-wsv-ext2 state) (1+ wsb)
                     (oss4-dx-ext2 state) (- dx0 1 sq4)
                     (oss4-dy-ext2 state) (- dy0 1 sq4)
                     (oss4-dz-ext2 state) (- dz0 1 sq4)
                     (oss4-dw-ext2 state) (- dw0 1 sq4))
              (if (not (zerop (logand c 1)))
                  (psetf (oss4-xsv-ext0 state) (+ xsb 2)
                         (oss4-xsv-ext1 state) (1+ xsb)
                         (oss4-dx-ext0 state) (- dx0 2 sq3)
                         (oss4-dx-ext1 state) (- dx0 1 sq3))
                  (psetf (oss4-xsv-ext0 state) xsb
                         (oss4-xsv-ext1 state) xsb
                         (oss4-dx-ext0 state) (- dx0 sq3)
                         (oss4-dx-ext1 state) (- dx0 sq3)))
              (cond
                ((not (zerop (logand c 2)))
                 (psetf (oss4-ysv-ext0 state) (1+ ysb)
                        (oss4-ysv-ext1 state) (1+ ysb)
                        (oss4-dy-ext0 state) (- dy0 1 sq3)
                        (oss4-dy-ext1 state) (- dy0 1 sq3))
                 (cond
                   ((zerop (logand c 1))
                    (incf (oss4-ysv-ext0 state))
                    (decf (oss4-dy-ext0 state)))
                   (t
                    (incf (oss4-ysv-ext1 state))
                    (decf (oss4-dy-ext1 state)))))
                (t
                 (psetf (oss4-ysv-ext0 state) ysb
                        (oss4-ysv-ext1 state) ysb
                        (oss4-dy-ext0 state) (- dy0 sq3)
                        (oss4-dy-ext1 state) (- dy0 sq3))))
              (cond
                ((not (zerop (logand c 4)))
                 (psetf (oss4-zsv-ext0 state) (1+ zsb)
                        (oss4-zsv-ext1 state) (1+ zsb)
                        (oss4-dz-ext0 state) (- dz0 1 sq3)
                        (oss4-dz-ext1 state) (- dz0 1 sq3))
                 (cond
                   ((zerop (logand c 3))
                    (incf (oss4-zsv-ext0 state))
                    (decf (oss4-dz-ext0 state)))
                   (t
                    (incf (oss4-zsv-ext1 state))
                    (decf (oss4-dz-ext1 state)))))
                (t
                 (psetf (oss4-zsv-ext0 state) zsb
                        (oss4-zsv-ext1 state) zsb
                        (oss4-dz-ext0 state) (- dz0 sq3)
                        (oss4-dz-ext1 state) (- dz0 sq3))))
              (cond
                ((not (zerop (logand c 8)))
                 (psetf (oss4-wsv-ext0 state) (1+ wsb)
                        (oss4-wsv-ext1 state) (+ wsb 2)
                        (oss4-dw-ext0 state) (- dw0 1 sq3)
                        (oss4-dw-ext1 state) (- dw0 2 sq3)))
                (t
                 (psetf (oss4-wsv-ext0 state) wsb
                        (oss4-wsv-ext1 state) wsb
                        (oss4-dw-ext0 state) (- dw0 sq3)
                        (oss4-dw-ext1 state) (- dw0 sq3))))))
        (let ((c1 (if a-bigger-p point-a point-b))
              (c2 (if a-bigger-p point-b point-a)))
          (if (not (zerop (logand c1 1)))
              (psetf (oss4-xsv-ext0 state) (+ xsb 2)
                     (oss4-xsv-ext1 state) (1+ xsb)
                     (oss4-dx-ext0 state) (- dx0 2 sq3)
                     (oss4-dx-ext1 state) (- dx0 1 sq3))
              (psetf (oss4-xsv-ext0 state) xsb
                     (oss4-xsv-ext1 state) xsb
                     (oss4-dx-ext0 state) (- dx0 sq3)
                     (oss4-dx-ext1 state) (- dx0 sq3)))
          (cond
            ((not (zerop (logand c1 2)))
             (psetf (oss4-ysv-ext0 state) (1+ ysb)
                    (oss4-ysv-ext1 state) (1+ ysb)
                    (oss4-dy-ext0 state) (- dy0 1 sq3)
                    (oss4-dy-ext1 state) (- dy0 1 sq3))
             (cond
               ((zerop (logand c1 1))
                (incf (oss4-ysv-ext0 state))
                (decf (oss4-dy-ext0 state)))
               (t
                (incf (oss4-ysv-ext1 state))
                (decf (oss4-dy-ext1 state)))))
            (t
             (psetf (oss4-ysv-ext0 state) ysb
                    (oss4-ysv-ext1 state) ysb
                    (oss4-dy-ext0 state) (- dy0 sq3)
                    (oss4-dy-ext1 state) (- dy0 sq3))))
          (cond
            ((not (zerop (logand c1 4)))
             (psetf (oss4-zsv-ext0 state) (1+ zsb)
                    (oss4-zsv-ext1 state) (1+ zsb)
                    (oss4-dz-ext0 state) (- dz0 1 sq3)
                    (oss4-dz-ext1 state) (- dz0 1 sq3))
             (cond
               ((zerop (logand c1 3))
                (incf (oss4-zsv-ext0 state))
                (decf (oss4-dz-ext0 state)))
               (t
                (incf (oss4-zsv-ext1 state))
                (decf (oss4-dz-ext1 state)))))
            (t
             (psetf (oss4-zsv-ext0 state) zsb
                    (oss4-zsv-ext1 state) zsb
                    (oss4-dz-ext0 state) (- dz0 sq3)
                    (oss4-dz-ext1 state) (- dz0 sq3))))
          (if (not (zerop (logand c1 8)))
              (psetf (oss4-wsv-ext0 state) (1+ wsb)
                     (oss4-wsv-ext1 state) (+ wsb 2)
                     (oss4-dw-ext0 state) (- dw0 1 sq3)
                     (oss4-dw-ext1 state) (- dw0 2 sq3))
              (psetf (oss4-wsv-ext0 state) wsb
                     (oss4-wsv-ext1 state) wsb
                     (oss4-dw-ext0 state) (- dw0 sq3)
                     (oss4-dw-ext1 state) (- dw0 sq3)))
          (psetf (oss4-xsv-ext2 state) (1+ xsb)
                 (oss4-ysv-ext2 state) (1+ ysb)
                 (oss4-zsv-ext2 state) (1+ zsb)
                 (oss4-wsv-ext2 state) (1+ wsb)
                 (oss4-dx-ext2 state) (- dx0 1 sq2)
                 (oss4-dy-ext2 state) (- dy0 1 sq2)
                 (oss4-dz-ext2 state) (- dz0 1 sq2)
                 (oss4-dw-ext2 state) (- dw0 1 sq2))
          (cond
            ((zerop (logand c2 1))
             (decf (oss4-xsv-ext2 state) 2)
             (incf (oss4-dx-ext2 state) 2))
            ((zerop (logand c2 2))
             (decf (oss4-ysv-ext2 state) 2)
             (incf (oss4-dy-ext2 state) 2))
            ((zerop (logand c2 4))
             (decf (oss4-zsv-ext2 state) 2)
             (incf (oss4-dz-ext2 state) 2))
            (t
             (decf (oss4-wsv-ext2 state) 2)
             (incf (oss4-dw-ext2 state) 2)))))
    (setf (oss4-dx4 state) (- dx0 1 sq3)
          (oss4-dy4 state) (- dy0 1 sq3)
          (oss4-dz4 state) (- dz0 1 sq3)
          (oss4-dw4 state) (- dw0 sq3)
          (oss4-dx3 state) (oss4-dx4 state)
          (oss4-dy3 state) (oss4-dy4 state)
          (oss4-dz3 state) (- dz0 sq3)
          (oss4-dw3 state) (- dw0 1 sq3)
          (oss4-dx2 state) (oss4-dx4 state)
          (oss4-dy2 state) (- dy0 sq3)
          (oss4-dz2 state) (oss4-dz4 state)
          (oss4-dw2 state) (oss4-dw3 state)
          (oss4-dx1 state) (- dx0 sq3)
          (oss4-dy1 state) (oss4-dy4 state)
          (oss4-dz1 state) (oss4-dz4 state)
          (oss4-dw1 state) (oss4-dw3 state)
          (oss4-dx5 state) (- dx0 1 sq2)
          (oss4-dy5 state) (- dy0 1 sq2)
          (oss4-dz5 state) (- dz0 sq2)
          (oss4-dw5 state) (- dw0 sq2)
          (oss4-dx6 state) (- dx0 1 sq2)
          (oss4-dy6 state) (- dy0 sq2)
          (oss4-dz6 state) (- dz0 1 sq2)
          (oss4-dw6 state) (- dw0 sq2)
          (oss4-dx7 state) (- dx0 1 sq2)
          (oss4-dy7 state) (- dy0 sq2)
          (oss4-dz7 state) (- dz0 sq2)
          (oss4-dw7 state) (- dw0 1 sq2)
          (oss4-dx8 state) (- dx0 sq2)
          (oss4-dy8 state) (- dy0 1 sq2)
          (oss4-dz8 state) (- dz0 1 sq2)
          (oss4-dw8 state) (- dw0 sq2)
          (oss4-dx9 state) (- dx0 sq2)
          (oss4-dy9 state) (- dy0 1 sq2)
          (oss4-dz9 state) (- dz0 sq2)
          (oss4-dw9 state) (- dw0 1 sq2)
          (oss4-dx10 state) (- dx0 sq2)
          (oss4-dy10 state) (- dy0 sq2)
          (oss4-dz10 state) (- dz0 1 sq2)
          (oss4-dw10 state) (- dw0 1 sq2))
    (values)))

(defun %open-simplex-4d (x y z w)
  (declare (optimize speed)
           (double-float x y z w))
  (let ((state (make-open-simplex-4d-state x y z w)))
    (cond
      ((<= (oss4-ins state) 1)
       (open-simplex-4d/in1 state)
       (open-simplex-4d/contribute1 state))
      ((>= (oss4-ins state) 3)
       (open-simplex-4d/in2 state)
       (open-simplex-4d/contribute2 state))
      ((<= (oss4-ins state) 2)
       (open-simplex-4d/in3 state)
       (open-simplex-4d/contribute3 state))
      (t
       (open-simplex-4d/in4 state)
       (open-simplex-4d/contribute4 state)))
    (open-simplex-4d/contribute5 state)
    (float (* (oss4-value state) +open-simplex-4d/scale+) 1f0)))

(defun open-simplex-4d (x y z w)
  (declare (real x y z w))
  (%open-simplex-4d (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))
