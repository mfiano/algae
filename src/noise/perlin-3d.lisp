(in-package #:net.mfiano.lisp.algae.noise)

(u:defun-inline %perlin-3d (x y z)
  (declare (optimize speed)
           (f50 x y z))
  (flet ((grad (hash x y z)
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
               (p +permutation+)
               (a (+ (aref p xi) yi))
               (b (+ (aref p (1+ xi)) yi)))
      (float
       (lerp w
             (lerp v
                   (lerp u
                         (grad (pget p zi a) xf yf zf)
                         (grad (pget p zi b) (1- xf) yf zf))
                   (lerp u
                         (grad (pget p zi (1+ a)) xf (1- yf) zf)
                         (grad (pget p zi (1+ b)) (1- xf) (1- yf) zf)))
             (lerp v
                   (lerp u
                         (grad (pget p (1+ zi) a) xf yf (1- zf))
                         (grad (pget p (1+ zi) b) (1- xf) yf (1- zf)))
                   (lerp u
                         (grad (pget p (1+ zi) (1+ a)) xf (1- yf) (1- zf))
                         (grad (pget p zi (1+ b)) (1- xf) (1- yf) (1- zf)))))
       1f0))))

(defun perlin-3d (x y z)
  (declare (real x y z))
  (%perlin-3d (float x 1d0) (float y 1d0) (float z 1d0)))
