(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.simplex-1d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.simplex-1d)

(u:define-constant +scale+ 0.395d0)

(u:defun-inline sample (x)
  (declare (optimize speed)
           (c:f50 x))
  (flet ((noise (hash x)
           (let* ((s (- 1 (* x x)))
                  (h (logand hash 15))
                  (grad (if (zerop (logand h 8))
                            (* (1+ (logand h 7)) x)
                            (* (- (1+ (logand h 7))) x))))
             (if (plusp s)
                 (* (expt s 4) grad)
                 0d0))))
    (let* ((i1 (floor x))
           (i2 (1+ i1))
           (x1 (- x i1))
           (x2 (1- x1))
           (p c:+perlin-permutation+)
           (n1 (noise (c:pget p i1) x1))
           (n2 (noise (c:pget p i2) x2)))
      (float (* (+ n1 n2) +scale+) 1f0))))
