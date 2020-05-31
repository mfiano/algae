(asdf:defsystem #:net.mfiano.lisp.algae
  :description "Assortment of Lisp Game Algorithms and Experiments"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/algae"
  :source-control (:git "https://github.com/mfiano/algae")
  :bug-tracker "https://github.com/mfiano/algae/issues"
  :encoding :utf-8
  :depends-on (#:cl-pcg
               #:ironclad
               #:global-vars
               #:net.mfiano.lisp.golden-utils
               #:net.mfiano.lisp.origin
               #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:module "doubly-linked-list"
    :components
    ((:file "doubly-linked-list")))
   (:module "avl-tree"
    :components
    ((:file "avl-tree")))
   (:module "quad-tree"
    :components
    ((:file "quad-tree")))
   (:module "tile-grid"
    :components
    ((:file "tile-grid")))
   (:module "convolution-kernel"
    :components
    ((:file "convolution-kernel")))
   (:module "rng-pool"
    :components
    ((:file "rng-pool")))
   (:module "uuid"
    :components
    ((:file "uuid")))
   (:module "noise"
    :components
    ((:file "package")
     (:file "common")
     (:file "perlin")
     (:file "simplex")
     (:file "open-simplex-2d")
     (:file "open-simplex-3d")))))
