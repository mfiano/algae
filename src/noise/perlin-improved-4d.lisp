(in-package #:net.mfiano.lisp.algae.noise)

(u:defun-inline %perlin-improved-4d (x y z w)
  (declare (optimize speed)
           (f50 x y z w))
  (flet ((grad (hash x y z w)
           (declare (f50 x y z w))
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
               (wi1 (1+ wi))
               (xf-1 (1- xf))
               (yf-1 (1- yf))
               (zf-1 (1- zf))
               (wf-1 (1- wf))
               (fs (fade xf))
               (ft (fade yf))
               (fr (fade zf))
               (fq (fade wf))
               (p +permutation+))
      (float (lerp
              fs
              (lerp
               ft
               (lerp
                fr
                (lerp fq
                      (grad (pget p xi yi zi wi) xf yf zf wf)
                      (grad (pget p xi yi zi wi1) xf yf zf wf-1))
                (lerp fq
                      (grad (pget p xi yi zi1 wi) xf yf zf-1 wf)
                      (grad (pget p xi yi zi1 wi1) xf yf zf-1 wf-1)))
               (lerp
                fr
                (lerp fq
                      (grad (pget p xi yi1 zi wi) xf yf-1 zf wf)
                      (grad (pget p xi yi1 zi wi1) xf yf-1 zf wf-1))
                (lerp fq
                      (grad (pget p xi yi1 zi1 wi) xf yf-1 zf-1 wf)
                      (grad (pget p xi yi1 zi1 wi1) xf yf-1 zf-1 wf-1))))
              (lerp
               ft
               (lerp
                fr
                (lerp fq
                      (grad (pget p xi1 yi zi wi) xf-1 yf zf wf)
                      (grad (pget p xi1 yi zi wi1) xf-1 yf zf wf-1))
                (lerp fq
                      (grad (pget p xi1 yi zi1 wi) xf-1 yf zf-1 wf)
                      (grad (pget p xi1 yi zi1 wi1) xf-1 yf zf-1 wf-1)))
               (lerp
                fr
                (lerp fq
                      (grad (pget p xi1 yi1 zi wi) xf-1 yf-1 zf wf)
                      (grad (pget p xi1 yi1 zi wi1) xf-1 yf-1 zf wf-1))
                (lerp fq
                      (grad (pget p xi1 yi1 zi1 wi) xf-1 yf-1 zf-1 wf)
                      (grad (pget p xi1 yi1 zi1 wi1) xf-1 yf-1 zf-1 wf-1)))))
             1f0))))

(defun perlin-improved-4d (x y z w)
  (declare (real x y z w))
  (%perlin-improved-4d (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))
