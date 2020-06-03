(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise
  (:local-nicknames
   (#:os2 #:net.mfiano.lisp.algae.noise.open-simplex-2d)
   (#:os3 #:net.mfiano.lisp.algae.noise.open-simplex-3d)
   (#:os4 #:net.mfiano.lisp.algae.noise.open-simplex-4d)
   (#:p1 #:net.mfiano.lisp.algae.noise.perlin-improved-1d)
   (#:p2 #:net.mfiano.lisp.algae.noise.perlin-improved-2d)
   (#:p3 #:net.mfiano.lisp.algae.noise.perlin-improved-3d)
   (#:p4 #:net.mfiano.lisp.algae.noise.perlin-improved-4d)
   (#:s1 #:net.mfiano.lisp.algae.noise.simplex-1d)
   (#:s2 #:net.mfiano.lisp.algae.noise.simplex-2d)
   (#:s3 #:net.mfiano.lisp.algae.noise.simplex-3d)
   (#:s4 #:net.mfiano.lisp.algae.noise.simplex-4d)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:open-simplex2
   #:open-simplex3
   #:open-simplex4
   #:perlin1
   #:perlin2
   #:perlin3
   #:perlin4
   #:simplex1
   #:simplex2
   #:simplex3
   #:simplex4))

(in-package #:net.mfiano.lisp.algae.noise)

;;; Noise functions

(defun perlin1 (x)
  (declare (real x))
  (p1:sample (float x 1d0)))

(defun perlin2 (x y)
  (declare (real x y))
  (p2:sample (float x 1d0) (float y 1d0)))

(defun perlin3 (x y z)
  (declare (real x y z))
  (p3:sample (float x 1d0) (float y 1d0) (float z 1d0)))

(defun perlin4 (x y z w)
  (declare (real x y z w))
  (p4:sample (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))

(defun simplex1 (x)
  (declare (real x))
  (s1:sample (float x 1d0)))

(defun simplex2 (x y)
  (declare (real x y))
  (s2:sample (float x 1d0) (float y 1d0)))

(defun simplex3 (x y z)
  (declare (real x y z))
  (s3:sample (float x 1d0) (float y 1d0) (float z 1d0)))

(defun simplex4 (x y z w)
  (declare (real x y z w))
  (s4:sample (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))

(defun open-simplex2 (x y)
  (declare (real x y))
  (os2:sample (float x 1d0) (float y 1d0)))

(defun open-simplex3 (x y z)
  (declare (real x y z))
  (os3:sample (float x 1d0) (float y 1d0) (float z 1d0)))

(defun open-simplex4 (x y z w)
  (declare (real x y z w))
  (os4:sample (float x 1d0) (float y 1d0) (float z 1d0) (float w 1d0)))
