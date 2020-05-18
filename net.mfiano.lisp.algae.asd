(asdf:defsystem #:net.mfiano.lisp.algae
  :description "Assortment of Lisp Game Algorithms and Experiments"
  :author ("Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://mfiano.net/projects/algae"
  :source-control (:git "https://github.com/mfiano/algae")
  :bug-tracker "https://github.com/mfiano/algae/issues"
  :encoding :utf-8
  :depends-on (#:net.mfiano.lisp.golden-utils
               #:net.mfiano.lisp.origin)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "avl-tree")
   (:file "doubly-linked-list")
   (:file "quad-tree")))
