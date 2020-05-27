(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:perlin1d
   #:perlin2d
   #:perlin3d
   #:perlin4d
   #:simplex1d
   #:simplex2d
   #:simplex3d
   #:simplex4d))
