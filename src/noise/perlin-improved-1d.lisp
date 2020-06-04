(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise.perlin-improved-1d
  (:local-nicknames
   (#:c #:net.mfiano.lisp.algae.noise.common)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:sample))

(in-package #:net.mfiano.lisp.algae.noise.perlin-improved-1d)

(u:defun-inline sample (x)
  (declare (optimize speed)
           (c:f50 x))
  (flet ((fade (x)
           (* x x x (+ (* x (- (* x 6) 15)) 10)))
         (grad (hash x)
           (let* ((h (logand hash 15))
                  (grad (1+ (logand h 7))))
             (if (zerop (logand h 8))
                 (* grad x)
                 (* (- grad) x)))))
    (u:mvlet* ((xi xf (truncate x))
               (xi (logand xi 255))
               (u (fade xf))
               (p c:+perlin-permutation+)
               (a (aref p xi))
               (b (aref p (1+ xi))))
      (float (* (u:lerp u (grad a xf) (grad b (1- xf))) 0.25) 1f0))))
