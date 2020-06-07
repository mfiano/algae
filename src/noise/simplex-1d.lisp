(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.simplex-1d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:rng #:net.mfiano.lisp.algae.rng)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.simplex-1d)

(u:define-constant +scale+ 0.395d0)

(defclass sampler (c:sampler)
  ((%table :reader table
           :initarg :table)))

(u:defun-inline sample (sampler x)
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
           (p (the (simple-array u:ub8 (512)) (table sampler)))
           (n1 (noise (c:pget p i1) x1))
           (n2 (noise (c:pget p i2) x2)))
      (float (* (+ n1 n2) +scale+) 1f0))))

(defmethod c:make-sampler ((type (eql :simplex-1d)) seed)
  (declare (ignore seed))
  (let* ((table (rng:shuffle 'c::rng c:+perlin-permutation+))
         (sampler (make-instance 'sampler :table table)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (declare (ignore y z w))
      (sample sampler x))))
