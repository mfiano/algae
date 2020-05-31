(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.noise
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:open-simplex-2d
   #:open-simplex-3d
   #:open-simplex-2d
   #:perlin-1d
   #:perlin-2d
   #:perlin-3d
   #:perlin-4d
   #:simplex-1d
   #:simplex-2d
   #:simplex-3d
   #:simplex-4d))
