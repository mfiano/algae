(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.perlin-improved-2d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:rng #:net.mfiano.lisp.algae.rng)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.perlin-improved-2d)

(u:defun-inline sample (table x y)
  (declare (optimize speed)
           (c:f50 x y))
  (flet ((fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10)))
         (grad (hash x y)
           (let* ((h (logand hash 7))
                  (u (if (< h 4) x y))
                  (v (if (< h 4) y x)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (u (fade xf))
               (v (fade yf))
               (p (the (simple-array u:ub8 (512)) table))
               (a (+ (aref p xi) yi))
               (aa (aref p a))
               (ab (aref p (1+ a)))
               (b (+ (aref p (1+ xi)) yi))
               (ba (aref p b))
               (bb (aref p (1+ b))))
      (float
       (u:lerp v
               (u:lerp u
                       (grad (c:pget p aa) xf yf)
                       (grad (c:pget p ba) (1- xf) yf))
               (u:lerp u
                       (grad (c:pget p ab) xf (1- yf))
                       (grad (c:pget p bb) (1- xf) (1- yf))))
       1f0))))

(defmethod c::%make-sampler-func ((type (eql :perlin-2d)))
  (let ((table (rng:shuffle 'c::rng c:+perlin-permutation+)))
    (lambda (x &optional (y 0d0) z w)
      (declare (ignore z w))
      (sample table x y))))
