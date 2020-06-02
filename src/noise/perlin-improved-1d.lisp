(in-package #:net.mfiano.lisp.algae.noise)

(u:defun-inline %perlin-improved-1d (x)
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
               (p +permutation+)
               (a (aref p xi))
               (b (aref p (1+ xi))))
      (float (lerp u (grad a x) (grad b (1- x))) 1f0))))

(defun perlin-improved-1d (x)
  (declare (real x))
  (%perlin-improved-1d (float x 1d0)))