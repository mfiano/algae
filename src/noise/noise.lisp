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
  (:shadow
   #:+
   #:-
   #:*
   #:abs
   #:expt
   #:max
   #:min)
  (:export
   #:+
   #:-
   #:*
   #:abs
   #:billow
   #:blend
   #:clamp
   #:displace
   #:expt
   #:fractal
   #:make-sampler
   #:max
   #:min
   #:negate
   #:power
   #:ridges
   #:rotate-point
   #:sample
   #:scale
   #:scale-point
   #:translate-point
   #:turbulence
   #:wavelength
   #:write-image))

(in-package #:net.mfiano.lisp.algae.noise)

(defun make-sampler (type)
  (lambda (x &optional (y 0d0) (z 0d0) (w 0d0))
    (case type
      (:perlin-1d
       (p1:sample x))
      (:perlin-2d
       (p2:sample x y))
      (:perlin-3d
       (p3:sample x y z))
      (:perlin-4d
       (p4:sample x y z w))
      (:simplex-1d
       (s1:sample x))
      (:simplex-2d
       (s2:sample x y))
      (:simplex-3d
       (s3:sample x y z))
      (:simplex-4d
       (s4:sample x y z w))
      (:open-simplex-2d
       (os2:sample x y))
      (:open-simplex-3d
       (os3:sample x y z))
      (:open-simplex-4d
       (os4:sample x y z w))
      (t
       (error "~s is not a valid sampler type." type)))))

(defun sample (sampler &rest args)
  (apply sampler (mapcar (lambda (x) (float x 1d0)) args)))

(defun + (sampler1 sampler2)
  (lambda (&rest args)
    (cl:+ (apply sampler1 args) (apply sampler2 args))))

(defun - (sampler1 sampler2)
  (lambda (&rest args)
    (cl:- (apply sampler1 args) (apply sampler2 args))))

(defun * (sampler1 sampler2)
  (lambda (&rest args)
    (cl:* (apply sampler1 args) (apply sampler2 args))))

(defun min (sampler1 sampler2)
  (lambda (&rest args)
    (cl:min (apply sampler1 args) (apply sampler2 args))))

(defun max (sampler1 sampler2)
  (lambda (&rest args)
    (cl:max (apply sampler1 args) (apply sampler2 args))))

(defun blend (sampler1 sampler2 control-sampler)
  (lambda (&rest args)
    (u:lerp (apply control-sampler args)
            (apply sampler1 args)
            (apply sampler2 args))))

(defun abs (sampler)
  (lambda (&rest args)
    (cl:abs (apply sampler args))))

(defun expt (sampler &optional (power 1.0))
  (lambda (&rest args)
    (1- (cl:* (cl:expt (cl:abs (cl:* (1+ (apply sampler args)) 0.5)) power)
              2))))

(defun power (sampler1 sampler2)
  (lambda (&rest args)
    (let ((sample1 (apply sampler1 args))
          (sample2 (apply sampler2 args)))
      (cl:* sample1 sample1 sample2 sample2))))

(defun clamp (sampler &key (min -1f0) (max 1f0))
  (lambda (&rest args)
    (u:clamp (apply sampler args) min max)))

(defun negate (sampler)
  (lambda (&rest args)
    (cl:- (apply sampler args))))

(defun scale (sampler &optional (scalar 1f0))
  (lambda (&rest args)
    (cl:* (apply sampler args) scalar)))

(defun scale-point (sampler &rest scalars)
  (lambda (&rest args)
    (loop :for arg :in args
          :for scalar :in scalars
          :collect (/ arg scalar) :into scale
          :finally (return (apply sampler scale)))))

(defun translate-point (sampler &rest offsets)
  (lambda (&rest args)
    (loop :for arg :in args
          :for offset :in offsets
          :collect (cl:+ arg offset) :into translation
          :finally (return (apply sampler translation)))))

(defun rotate-point (sampler &key (x 0.0) (y 0.0) (z 0.0))
  (let* ((cx (cos x))
         (cy (cos y))
         (cz (cos z))
         (sx (sin x))
         (sy (sin y))
         (sz (sin z))
         (x1 (cl:+ (cl:* sy sx sz) (cl:* cy cz)))
         (y1 (cl:* cx sz))
         (z1 (cl:- (cl:* sy cz) (cl:* cy sy sz)))
         (x2 (cl:- (cl:* sy sx cz) (cl:* cy sz)))
         (y2 (cl:* cx cz))
         (z2 (cl:- (cl:* (cl:- cy) sx cz) (cl:* sy sz)))
         (x3 (cl:* (cl:- sy) cx))
         (y3 sx)
         (z3 (cl:* cy cx)))
    (lambda (x &optional (y 0d0) (z 0d0))
      (let ((x (cl:+ (cl:* x x1) (cl:* y y1) (cl:* z z1)))
            (y (cl:+ (cl:* x x2) (cl:* y y2) (cl:* z z2)))
            (z (cl:+ (cl:* x x3) (cl:* y y3) (cl:* z z3))))
        (funcall sampler x y z)))))

(defun wavelength (sampler &optional (scalar 1.0))
  (lambda (&rest args)
    (apply sampler (mapcar (lambda (x) (/ x scalar)) args))))

(defun displace (sampler &rest displacement-samplers)
  (lambda (&rest args)
    (loop :for arg :in args
          :for displace-sampler :in displacement-samplers
          :collect (cl:+ (apply displace-sampler args) arg) :into displacements
          :finally (return (apply sampler displacements)))))

(defun fractal (sampler
                &key (octaves 4) (frequency 1.0) (gain 0.5) (lacunarity 2.0))
  (lambda (&rest args)
    (loop :with amplitude = 1.0
          :with args = (mapcar (lambda (x) (cl:* x frequency)) args)
          :repeat octaves
          :for sample = (apply sampler args)
          :sum (cl:* sample amplitude) :into value
          :do (setf args (mapcar (lambda (x) (cl:* x lacunarity)) args)
                    amplitude (cl:* amplitude gain))
          :finally (return value))))

(defun ridges (sampler
               &key (octaves 4) (frequency 1.0) (gain 2.0) (lacunarity 2.0)
                 (exponent 1.0) (offset 1.0))
  (let ((weights (make-array octaves :element-type 'single-float)))
    (loop :for i :below octaves
          :for frequency = 1.0 :then (cl:* frequency lacunarity)
          :do (setf (aref weights i) (cl:expt frequency (cl:- exponent))))
    (lambda (&rest args)
      (let ((args (mapcar (lambda (x) (cl:* x frequency)) args))
            (weight 1.0)
            (value 0.0))
        (dotimes (i octaves)
          (let* ((sample (cl:- offset (cl:abs (apply sampler args))))
                 (sample (cl:* sample sample weight)))
            (incf value (cl:* sample (aref weights i)))
            (setf weight (u:clamp (cl:* sample gain) 0.0 1.0)
                  args (mapcar (lambda (x) (cl:* x lacunarity)) args))))
        (1- (cl:* value 1.25))))))

(defun billow (sampler
               &key (octaves 4) (frequency 1.0) (gain 0.5) (lacunarity 2.0))
  (lambda (&rest args)
    (loop :with amplitude = 1.0
          :with args = (mapcar (lambda (x) (cl:* x frequency)) args)
          :repeat octaves
          :for sample = (1- (cl:* (cl:abs (apply sampler args)) 2))
          :sum (cl:* sample amplitude) :into value
          :do (setf args (mapcar (lambda (x) (cl:* x lacunarity)) args)
                    amplitude (cl:* amplitude gain))
          :finally (return (cl:+ value 0.5)))))

(defun turbulence (sampler &key (frequency 1.0) (power 1.0) (roughness 3))
  (let ((distortion (fractal (make-sampler :perlin-3d)
                             :octaves roughness
                             :frequency frequency)))
    (lambda (&rest args)
      (destructuring-bind (x &optional (y 0d0) (z 0d0) (w 0d0)) args
        (let* ((x0 (cl:+ x 0.1894226))
               (y0 (cl:+ y 0.9937134))
               (z0 (cl:+ z 0.47816467))
               (w0 (cl:+ w 0.78841865))
               (x1 (cl:+ x 0.40464783))
               (y1 (cl:+ y 0.27661133))
               (z1 (cl:+ z 0.9230499))
               (w1 (cl:+ w 0.5960642))
               (x2 (cl:+ x 0.821228))
               (y2 (cl:+ y 0.1710968))
               (z2 (cl:+ z 0.6842804))
               (w2 (cl:+ w 0.04850936))
               (x3 (cl:+ x 0.80564606))
               (y3 (cl:+ y 0.7283617))
               (z3 (cl:+ z 0.69029343))
               (w3 (cl:+ w 0.17203021))
               (dx (cl:+ x (cl:* (sample distortion x0 y0 z0 w0) power)))
               (dy (cl:+ y (cl:* (sample distortion x1 y1 z1 w1) power)))
               (dz (cl:+ z (cl:* (sample distortion x2 y2 z2 w2) power)))
               (dw (cl:+ w (cl:* (sample distortion x3 y3 z3 w3) power))))
          (funcall sampler dx dy dz dw))))))

(defun write-image (out-file sampler
                    &key (width 1024) (height 1024) (r 1.0) (g 1.0) (b 1.0))
  (let* ((png (make-instance 'zpng:png
                             :color-type :truecolor
                             :width width
                             :height height))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((sample (sample sampler x y))
               (sample (cl:+ (cl:* sample 0.5) 0.5))
               (red (u:clamp (floor (cl:* sample r 255)) 0 255))
               (green (u:clamp (floor (cl:* sample g 255)) 0 255))
               (blue (u:clamp (floor (cl:* sample b 255)) 0 255)))
          (setf (aref data y x 0) red
                (aref data y x 1) green
                (aref data y x 2) blue))))
    (zpng:write-png png out-file)))
