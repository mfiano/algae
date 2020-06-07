(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.simplex-4d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:rng #:net.mfiano.lisp.algae.rng)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.simplex-4d)

(u:define-constant +skew-factor+ (/ (1- (sqrt 5d0)) 4))

(u:define-constant +unskew-factor+ (/ (- 5 (sqrt 5d0)) 20))

(u:define-constant +scale+ 27d0)

(u:define-constant +simplex-table+
    (make-array 256
                :element-type 'u:ub8
                :initial-contents
                '(0 1 2 3 0 1 3 2 0 0 0 0 0 2 3 1 0 0 0 0 0 0 0 0 0 0 0 0 1 2 3
                  0 0 2 1 3 0 0 0 0 0 3 1 2 0 3 2 1 0 0 0 0 0 0 0 0 0 0 0 0 1 3
                  2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                  0 0 0 1 2 0 3 0 0 0 0 1 3 0 2 0 0 0 0 0 0 0 0 0 0 0 0 2 3 0 1
                  2 3 1 0 1 0 2 3 1 0 3 2 0 0 0 0 0 0 0 0 0 0 0 0 2 0 3 1 0 0 0
                  0 2 1 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                  0 0 0 0 0 0 2 0 1 3 0 0 0 0 0 0 0 0 0 0 0 0 3 0 1 2 3 0 2 1 0
                  0 0 0 3 1 2 0 2 1 0 3 0 0 0 0 0 0 0 0 0 0 0 0 3 1 0 2 0 0 0 0
                  3 2 0 1 3 2 1 0))
  :test #'equalp)

(u:defun-inline sample (table x y z w)
  (declare (optimize speed)
           (c:f50 x y z w))
  (flet ((get-simplex (x y z w)
           (let* ((c1 (if (> x y) 32 0))
                  (c2 (if (> x z) 16 0))
                  (c3 (if (> y z) 8 0))
                  (c4 (if (> x w) 4 0))
                  (c5 (if (> y w) 2 0))
                  (c6 (if (> z w) 1 0))
                  (c (* 4 (+ c1 c2 c3 c4 c5 c6)))
                  (a0 (aref +simplex-table+ (+ c 0)))
                  (a1 (aref +simplex-table+ (+ c 1)))
                  (a2 (aref +simplex-table+ (+ c 2)))
                  (a3 (aref +simplex-table+ (+ c 3))))
             (values (if (>= a0 3) 1 0)
                     (if (>= a1 3) 1 0)
                     (if (>= a2 3) 1 0)
                     (if (>= a3 3) 1 0)
                     (if (>= a0 2) 1 0)
                     (if (>= a1 2) 1 0)
                     (if (>= a2 2) 1 0)
                     (if (>= a3 2) 1 0)
                     (if (>= a0 1) 1 0)
                     (if (>= a1 1) 1 0)
                     (if (>= a2 1) 1 0)
                     (if (>= a3 1) 1 0))))
         (noise (hash x y z w)
           (let* ((s (- 0.6 (* x x) (* y y) (* z z) (* w w)))
                  (h (logand hash 31))
                  (u (if (< h 24) x y))
                  (v (if (< h 16) y z))
                  (w (if (< h 8) z w))
                  (grad (+ (if (zerop (logand h 1)) u (- u))
                           (if (zerop (logand h 2)) v (- v))
                           (if (zerop (logand h 4)) w (- w)))))
             (if (plusp s)
                 (* (expt s 4) grad)
                 0d0))))
    (u:mvlet* ((s (* (+ x y z w) +skew-factor+))
               (i (floor (+ x s)))
               (j (floor (+ y s)))
               (k (floor (+ z s)))
               (l (floor (+ w s)))
               (tx (* (+ i j k l) +unskew-factor+))
               (x1 (- x (- i tx)))
               (y1 (- y (- j tx)))
               (z1 (- z (- k tx)))
               (w1 (- w (- l tx)))
               (i1 j1 k1 l1 i2 j2 k2 l2 i3 j3 k3 l3 (get-simplex x1 y1 z1 w1))
               (x2 (+ (- x1 i1) +unskew-factor+))
               (y2 (+ (- y1 j1) +unskew-factor+))
               (z2 (+ (- z1 k1) +unskew-factor+))
               (w2 (+ (- w1 l1) +unskew-factor+))
               (x3 (+ (- x1 i2) (* +unskew-factor+ 2)))
               (y3 (+ (- y1 j2) (* +unskew-factor+ 2)))
               (z3 (+ (- z1 k2) (* +unskew-factor+ 2)))
               (w3 (+ (- w1 l2) (* +unskew-factor+ 2)))
               (x4 (+ (- x1 i3) (* +unskew-factor+ 3)))
               (y4 (+ (- y1 j3) (* +unskew-factor+ 3)))
               (z4 (+ (- z1 k3) (* +unskew-factor+ 3)))
               (w4 (+ (- w1 l3) (* +unskew-factor+ 3)))
               (x5 (+ (1- x1) (* +unskew-factor+ 4)))
               (y5 (+ (1- y1) (* +unskew-factor+ 4)))
               (z5 (+ (1- z1) (* +unskew-factor+ 4)))
               (w5 (+ (1- w1) (* +unskew-factor+ 4)))
               (p (the (simple-array u:ub8 (512)) table))
               (g1 (c:pget p i j k l))
               (g2 (c:pget p (+ i i1) (+ j j1) (+ k k1) (+ l l1)))
               (g3 (c:pget p (+ i i2) (+ j j2) (+ k k2) (+ l l2)))
               (g4 (c:pget p (+ i i3) (+ j j3) (+ k k3) (+ l l3)))
               (g5 (c:pget p (1+ i) (1+ j) (1+ k) (1+ l)))
               (n1 (noise g1 x1 y1 z1 w1))
               (n2 (noise g2 x2 y2 z2 w2))
               (n3 (noise g3 x3 y3 z3 w3))
               (n4 (noise g4 x4 y4 z4 w4))
               (n5 (noise g5 x5 y5 z5 w5)))
      (float (* (+ n1 n2 n3 n4 n5) +scale+) 1f0))))

(defmethod c::%make-sampler-func ((type (eql :simplex-4d)))
  (let ((table (rng:shuffle 'c::rng c:+perlin-permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (sample table x y z w))))
