(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.perlin-improved-3d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:rng #:net.mfiano.lisp.algae.rng)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.perlin-improved-3d)

(u:defun-inline sample (table x y z)
  (declare (optimize speed)
           (c:f50 x y z))
  (flet ((fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10)))
         (grad (hash x y z)
           (let* ((h (logand hash 15))
                  (u (if (< h 8) x y))
                  (v (case h
                       ((0 1 2 3) y)
                       ((12 14) x)
                       (t z))))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (u (fade xf))
               (v (fade yf))
               (w (fade zf))
               (p (the (simple-array u:ub8 (512)) table))
               (a (+ (aref p xi) yi))
               (b (+ (aref p (1+ xi)) yi)))
      (float
       (u:lerp
        w
        (u:lerp
         v
         (u:lerp u
                 (grad (c:pget p zi a) xf yf zf)
                 (grad (c:pget p zi b) (1- xf) yf zf))
         (u:lerp u
                 (grad (c:pget p zi (1+ a)) xf (1- yf) zf)
                 (grad (c:pget p zi (1+ b)) (1- xf) (1- yf) zf)))
        (u:lerp
         v
         (u:lerp u
                 (grad (c:pget p (1+ zi) a) xf yf (1- zf))
                 (grad (c:pget p (1+ zi) b) (1- xf) yf (1- zf)))
         (u:lerp u
                 (grad (c:pget p (1+ zi) (1+ a)) xf (1- yf) (1- zf))
                 (grad (c:pget p zi (1+ b)) (1- xf) (1- yf) (1- zf)))))
       1f0))))

(defmethod c::%make-sampler-func ((type (eql :perlin-3d)))
  (let* ((table (rng:shuffle 'c::rng c:+perlin-permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) w)
      (declare (ignore w))
      (sample table x y z))))
