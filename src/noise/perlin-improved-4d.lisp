(in-package #:cl-user)

(defpackage #:algae.noise.perlin-improved-4d
  (:local-nicknames
   (#:c #:algae.noise.common)
   (#:rng #:algae.rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:algae.noise.perlin-improved-4d)

(u:defun-inline sample (table x y z w)
  (declare (optimize speed)
           (c:f50 x y z w))
  (flet ((fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10)))
         (grad (hash x y z w)
           (let* ((h (logand hash 31))
                  (u (if (< h 24) x y))
                  (v (if (< h 16) y z))
                  (w (if (< h 8) z w)))
             (+ (if (zerop (logand h 1)) u (- u))
                (if (zerop (logand h 2)) v (- v))
                (if (zerop (logand h 4)) w (- w))))))
    (u:mvlet* ((xi xf (truncate x))
               (yi yf (truncate y))
               (zi zf (truncate z))
               (wi wf (truncate w))
               (xi (logand xi 255))
               (yi (logand yi 255))
               (zi (logand zi 255))
               (wi (logand wi 255))
               (xi1 (logand (1+ xi) 255))
               (yi1 (logand (1+ yi) 255))
               (zi1 (logand (1+ zi) 255))
               (wi1 (logand (1+ wi) 255))
               (xf-1 (1- xf))
               (yf-1 (1- yf))
               (zf-1 (1- zf))
               (wf-1 (1- wf))
               (fs (fade xf))
               (ft (fade yf))
               (fr (fade zf))
               (fq (fade wf))
               (p (the (simple-array u:ub8 (512)) table)))
      (float
       (u:lerp
        fs
        (u:lerp
         ft
         (u:lerp
          fr
          (u:lerp fq
                  (grad (c:pget p xi yi zi wi) xf yf zf wf)
                  (grad (c:pget p xi yi zi wi1) xf yf zf wf-1))
          (u:lerp fq
                  (grad (c:pget p xi yi zi1 wi) xf yf zf-1 wf)
                  (grad (c:pget p xi yi zi1 wi1) xf yf zf-1 wf-1)))
         (u:lerp
          fr
          (u:lerp fq
                  (grad (c:pget p xi yi1 zi wi) xf yf-1 zf wf)
                  (grad (c:pget p xi yi1 zi wi1) xf yf-1 zf wf-1))
          (u:lerp fq
                  (grad (c:pget p xi yi1 zi1 wi) xf yf-1 zf-1 wf)
                  (grad (c:pget p xi yi1 zi1 wi1) xf yf-1 zf-1 wf-1))))
        (u:lerp
         ft
         (u:lerp
          fr
          (u:lerp fq
                  (grad (c:pget p xi1 yi zi wi) xf-1 yf zf wf)
                  (grad (c:pget p xi1 yi zi wi1) xf-1 yf zf wf-1))
          (u:lerp fq
                  (grad (c:pget p xi1 yi zi1 wi) xf-1 yf zf-1 wf)
                  (grad (c:pget p xi1 yi zi1 wi1) xf-1 yf zf-1 wf-1)))
         (u:lerp
          fr
          (u:lerp fq
                  (grad (c:pget p xi1 yi1 zi wi) xf-1 yf-1 zf wf)
                  (grad (c:pget p xi1 yi1 zi wi1) xf-1 yf-1 zf wf-1))
          (u:lerp fq
                  (grad (c:pget p xi1 yi1 zi1 wi) xf-1 yf-1 zf-1 wf)
                  (grad (c:pget p xi1 yi1 zi1 wi1) xf-1 yf-1 zf-1 wf-1)))))
       1f0))))

(defmethod c::%make-sampler-func ((type (eql :perlin-4d)))
  (let ((table (rng:shuffle 'c::rng c:+perlin-permutation+)))
    (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
      (sample table x y z w))))
