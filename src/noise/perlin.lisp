(in-package #:net.mfiano.lisp.algae.noise)

;;;; Improved Perlin noise

(u:defun-inline %perlin1d (x)
  (declare (optimize speed)
           (f50 x))
  (flet ((grad (hash x)
           (let* ((h (logand hash 15))
                  (grad (1+ (logand h 7))))
             (if (zerop (logand h 8))
                 (* grad x)
                 (* (- grad) x)))))
    (u:mvlet* ((xi xf (truncate x))
               (xi (logand xi 255))
               (u (fade xf))
               (a (aref +p+ xi))
               (b (aref +p+ (1+ xi))))
      (float (lerp u (grad a x) (grad b (1- x))) 1f0))))

(defun perlin1d (x)
  (declare (real x))
  (%perlin1d (float x 1d0)))

(u:defun-inline %perlin2d (x y)
  (declare (optimize speed)
           (f50 x y))
  (flet ((grad (hash x y)
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
               (a (+ (aref +p+ xi) yi))
               (aa (aref +p+ a))
               (ab (aref +p+ (1+ a)))
               (b (+ (aref +p+ (1+ xi)) yi))
               (ba (aref +p+ b))
               (bb (aref +p+ (1+ b))))
      (float
       (lerp v
             (lerp u
                   (grad (pget aa) xf yf)
                   (grad (pget ba) (1- xf) yf))
             (lerp u
                   (grad (pget ab) xf (1- yf))
                   (grad (pget bb) (1- xf) (1- yf))))
       1f0))))

(defun perlin2d (x y)
  (declare (real x y))
  (%perlin2d (float x 1d0) (float y 1d0)))

(u:defun-inline %perlin3d (x y z)
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
               (a (+ (aref +p+ xi) yi))
               (b (+ (aref +p+ (1+ xi)) yi)))
      (float
       (lerp w
             (lerp v
                   (lerp u
                         (grad (pget zi a) xf yf zf)
                         (grad (pget zi b) (1- xf) yf zf))
                   (lerp u
                         (grad (pget zi (1+ a)) xf (1- yf) zf)
                         (grad (pget zi (1+ b)) (1- xf) (1- yf) zf)))
             (lerp v
                   (lerp u
                         (grad (pget (1+ zi) a) xf yf (1- zf))
                         (grad (pget (1+ zi) b) (1- xf) yf (1- zf)))
                   (lerp u
                         (grad (pget (1+ zi) (1+ a)) xf (1- yf) (1- zf))
                         (grad (pget zi (1+ b)) (1- xf) (1- yf) (1- zf)))))
       1f0))))

(defun perlin3d (x y z)
  (declare (real x y z))
  (%perlin3d (float x 1d0) (float y 1d0) (float z 1d0)))

(u:defun-inline %perlin4d (x y z w)
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
               (fq (fade wf)))
      (float (lerp
              fs
              (lerp
               ft
               (lerp
                fr
                (lerp fq
                      (grad (pget xi yi zi wi) xf yf zf wf)
                      (grad (pget xi yi zi wi1) xf yf zf wf-1))
                (lerp fq
                      (grad (pget xi yi zi1 wi) xf yf zf-1 wf)
                      (grad (pget xi yi zi1 wi1) xf yf zf-1 wf-1)))
               (lerp
                fr
                (lerp fq
                      (grad (pget xi yi1 zi wi) xf yf-1 zf wf)
                      (grad (pget xi yi1 zi wi1) xf yf-1 zf wf-1))
                (lerp fq
                      (grad (pget xi yi1 zi1 wi) xf yf-1 zf-1 wf)
                      (grad (pget xi yi1 zi1 wi1) xf yf-1 zf-1 wf-1))))
              (lerp
               ft
               (lerp
                fr
                (lerp fq
                      (grad (pget xi1 yi zi wi) xf-1 yf zf wf)
                      (grad (pget xi1 yi zi wi1) xf-1 yf zf wf-1))
                (lerp fq
                      (grad (pget xi1 yi zi1 wi) xf-1 yf zf-1 wf)
                      (grad (pget xi1 yi zi1 wi1) xf-1 yf zf-1 wf-1)))
               (lerp
                fr
                (lerp fq
                      (grad (pget xi1 yi1 zi wi) xf-1 yf-1 zf wf)
                      (grad (pget xi1 yi1 zi wi1) xf-1 yf-1 zf wf-1))
                (lerp fq
                      (grad (pget xi1 yi1 zi1 wi) xf-1 yf-1 zf-1 wf)
                      (grad (pget xi1 yi1 zi1 wi1) xf-1 yf-1 zf-1 wf-1)))))
             1f0))))

(defun perlin4d (x y z w)
  (declare (real x y z w))
  (%perlin4d (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))
