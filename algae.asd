(asdf:defsystem #:algae
  :description "Assortment of Lisp Game Algorithms and Experiments"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://github.com/mfiano/algae"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:mfiano-utils
               #:origin)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:module "data-structures"
    :components
    ((:file "avl-tree")
     (:file "binary-search-tree")
     (:file "doubly-linked-list")
     (:file "dynamic-array")
     (:file "identifier-pool")
     (:file "quad-tree")
     (:file "slot-map")
     (:file "sparse-set")))
   (:module "tile-grid"
    :components
    ((:file "tile-grid")))
   (:module "convolution-kernel"
    :components
    ((:file "convolution-kernel")))
   (:module "grid"
    :components
    ((:file "package")
     (:file "grid")
     (:file "quad")
     (:file "quad-4-way")
     (:file "quad-8-way")
     (:file "hex")
     (:file "hex-rows")
     (:file "hex-columns")))))
