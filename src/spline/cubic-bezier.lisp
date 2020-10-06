(in-package #:cl-user)

(defpackage #:net.mfiano.lisp.algae.spline.cubic-bezier
  (:local-nicknames
   (#:avl #:net.mfiano.lisp.algae.data-structures.avl-tree)
   (#:dv3 #:net.mfiano.lisp.origin.dvec3)
   (#:dv4 #:net.mfiano.lisp.origin.dvec4)
   (#:dm4 #:net.mfiano.lisp.origin.dmat4)
   (#:m4 #:net.mfiano.lisp.origin.mat4)
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:v3 #:net.mfiano.lisp.origin.vec3)
   (#:v4 #:net.mfiano.lisp.origin.vec4))
  (:use #:cl)
  (:export
   #:add-points
   #:collect-points
   #:collect-segments
   #:evaluate
   #:make-curve))

(in-package #:net.mfiano.lisp.algae.spline.cubic-bezier)

(u:define-constant +matrix+
    (dm4:mat -1 3 -3 1 3 -6 3 0 -3 3 0 0 1 0 0 0) :test #'equalp)

(defstruct (bezier-curve
            (:constructor %make-curve)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (subdivisions 100 :type fixnum)
  (geometry (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (arc-lengths (make-array 0) :type simple-array))

(defun estimate-arc-lengths (spline)
  (setf (aref (arc-lengths spline) 0) 0f0)
  (loop :with max = (subdivisions spline)
        :for i :from 1 :to max
        :for previous = (evaluate spline 0) :then current
        :for current = (evaluate spline (/ i max))
        :sum (v3:distance previous current) :into length
        :do (setf (aref (arc-lengths spline) i) length)))

(defun verify-points (points)
  (unless (every (lambda (x) (typep x 'v3:vec)) points)
    (error "Points must be a list of 3-component vectors.")))

(defun add-geometry (spline points)
  (loop :with segment-count = (1+ (/ (- (length points) 4) 3))
        :for (a b c d) :on points :by #'cdddr
        :for index :from 0
        :when (< index segment-count)
          :do (vector-push-extend
               (dm4:mat (m4:mat (v4:vec a) (v4:vec b) (v4:vec c) (v4:vec d)))
               (geometry spline)))
  (estimate-arc-lengths spline))

(defun make-geometry (spline points)
  (let ((point-count (length points)))
    (unless (= 1 (mod point-count 3))
      (error "Invalid number of points: ~s." point-count))
    (verify-points points)
    (add-geometry spline points)))

(defun add-points (spline points)
  (let ((point-count (length points)))
    (unless (and (plusp point-count)
                 (zerop (mod point-count 3)))
      (error "Invalid number of points: ~s." point-count))
    (verify-points points)
    (let* ((geometry (geometry spline))
           (last-index (1- (length geometry)))
           (shared-point (v3:vec
                          (dv3:vec
                           (dm4:get-column (aref geometry last-index) 3)))))
      (add-geometry spline (cons shared-point points)))))

(defun make-curve (points &key (subdivisions 100))
  (unless (evenp subdivisions)
    (error "Subdivision count must be even."))
  (let* ((arc-lengths (make-array (1+ subdivisions)
                                  :element-type 'single-float))
         (spline (%make-curve :subdivisions subdivisions
                              :arc-lengths arc-lengths)))
    (make-geometry spline points)
    spline))

(defun remap (spline parameter)
  (flet ((%bisect (arc-lengths arc-length-count target)
           (loop :with high = (1- arc-length-count)
                 :with low = 0
                 :while (< low high)
                 :for index = 0 :then (+ low (floor (- high low) 2))
                 :do (if (< (aref arc-lengths index) target)
                         (setf low (1+ index))
                         (setf high index))
                 :finally (return index)))
         (%remap (arc-lengths arc-length-count target index)
           (let* ((before (aref arc-lengths index))
                  (after (aref arc-lengths (1+ index)))
                  (length (- after before))
                  (fraction (/ (- target before) length)))
             (max 0
                  (if (= before target)
                      (/ index (1- arc-length-count))
                      (/ (+ index fraction) (1- arc-length-count)))))))
    (let* ((arc-lengths (arc-lengths spline))
           (arc-length-count (length arc-lengths))
           (target (* (aref arc-lengths (1- arc-length-count)) parameter))
           (index (%bisect arc-lengths arc-length-count target)))
      (%remap arc-lengths arc-length-count target index))))

(defun evaluate (spline parameter &key even-spacing)
  (unless (<= 0 parameter 1)
    (error "Parameter must be in the closed range [0..1]."))
  (u:mvlet* ((geometry (geometry spline))
             (geometry-length (length geometry))
             (parameter (if even-spacing (remap spline parameter) parameter))
             (index x (floor (* parameter geometry-length))))
    (when (and (= index geometry-length)
               (zerop x))
      (decf index)
      (setf x 1))
    (v3:vec
     (dv3:vec
      (dm4:*v4 (dm4:* (aref geometry index) +matrix+)
               (dv4:vec (* x x x) (* x x) x 1))))))

(defun collect-points (curve count &key even-spacing)
  (loop :for i :below count
        :collect (evaluate curve (/ i (1- count)) :even-spacing even-spacing)))

(defun collect-segments (curve count &key even-spacing)
  (loop :for i :from 1 :to count
        :for p1 = (evaluate curve 0 :even-spacing even-spacing) :then p2
        :for p2 = (evaluate curve (/ i count) :even-spacing even-spacing)
        :collect (list p1 p2)))
