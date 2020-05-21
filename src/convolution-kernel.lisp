(in-package #:cl-user)

;;;; An implementation of a kernel useful for convolution-like cellular
;;;; transformations across a grid, not unlike techniques used in image
;;;; processing filters.

(defpackage #:net.mfiano.lisp.algae.convolution-kernel
  (:local-nicknames
   (#:tg #:net.mfiano.lisp.algae.tile-grid)
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:count
   #:find
   #:map)
  (:export
   #:cell->kernel
   #:convolve
   #:count
   #:detect
   #:filter
   #:find
   #:layout
   #:map
   #:origin
   #:process
   #:select))

(in-package #:net.mfiano.lisp.algae.convolution-kernel)

(defstruct (kernel
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (grid (tg::%make-grid) :type tg:grid)
  (origin-x 0 :type u:ub16)
  (origin-y 0 :type u:ub16)
  (min-x 0 :type u:ub8)
  (max-x 1 :type u:ub8)
  (min-y 0 :type u:ub8)
  (max-y 1 :type u:ub8)
  (selector (constantly nil) :type function)
  (mapper (constantly nil) :type function))

(u:fn-> make-kernel-factory (function function u:ub16 u:ub16 u:ub16 u:ub16)
        function)
(defun make-kernel-factory (selector mapper min-x max-x min-y max-y)
  (lambda (grid x y)
    (make-kernel :grid grid
                 :origin-x x
                 :origin-y y
                 :min-x min-x
                 :max-x max-x
                 :min-y min-y
                 :max-y max-y
                 :selector selector
                 :mapper mapper)))

(u:fn-> select/rect (kernel u:b32 u:b32) boolean)
(defun select/rect (kernel x y)
  (declare (optimize speed))
  (let ((min-x (min-x kernel))
        (max-x (max-x kernel))
        (min-y (min-y kernel))
        (max-y (max-y kernel)))
    (and (>= x (- max-x))
         (>= y (- max-y))
         (<= x max-x)
         (<= y max-y)
         (not (and (> x (- min-x))
                   (> y (- min-y))
                   (< x min-x)
                   (< y min-y))))))

(u:fn-> select/+ (kernel u:b32 u:b32) boolean)
(defun select/+ (kernel x y)
  (declare (optimize speed))
  (or (and (zerop y)
           (<= (min-x kernel) (abs x) (max-x kernel)))
      (and (zerop x)
           (<= (min-y kernel) (abs y) (max-y kernel)))))

(u:fn-> select/ellipse (kernel u:b32 u:b32) boolean)
(defun select/ellipse (kernel x y)
  (declare (optimize speed))
  (let ((max-x (max-x kernel))
        (max-y (max-y kernel)))
    (<= (+ (/ (expt x 2) (expt (/ max-x 2) 2f0))
           (/ (expt y 2) (expt (/ max-y 2) 2f0)))
        1f0)))

(u:fn-> select/x (kernel u:b32 u:b32) boolean)
(defun select/x (kernel x y)
  (declare (optimize speed))
  (let ((max (max (max-x kernel)
                  (max-y kernel))))
    (and (<= (abs x) max)
         (<= (abs y) max)
         (= (abs x) (abs y)))))

(u:fn-> select/h (kernel u:b32 u:b32) boolean)
(defun select/h (kernel x y)
  (declare (optimize speed))
  (and (zerop y)
       (<= (abs x) (max-x kernel))))

(u:fn-> select/v (kernel u:b32 u:b32) boolean)
(defun select/v (kernel x y)
  (declare (optimize speed))
  (and (zerop x)
       (<= (abs y) (max-y kernel))))

(u:fn-> select (kernel u:b32 u:b32) (or tg:cell null))
(defun select (kernel x y)
  (declare (optimize speed))
  (when (funcall (selector kernel) kernel x y)
    (let ((x (truncate (+ (origin-x kernel) x)))
          (y (truncate (+ (origin-y kernel) y))))
      (tg:get-cell (grid kernel) x y))))

(u:fn-> map/rect (kernel function) list)
(defun map/rect (kernel func)
  (declare (optimize speed))
  (loop :with result = nil
        :with max-x = (max-x kernel)
        :with max-y = (max-y kernel)
        :for y :from max-y :downto (- max-y)
        :do (loop :for x :from (- max-x) :to max-x
                  :for cell = (select kernel x y)
                  :when cell
                    :do (push (funcall func cell) result))
        :finally (return result)))

(u:fn-> map/+ (kernel function) list)
(defun map/+ (kernel func)
  (declare (optimize speed))
  (let ((results)
        (max-x (max-x kernel))
        (max-y (max-y kernel)))
    (loop :for y :from (- max-y) :to max-y
          :for cell = (select kernel 0 y)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from (- max-x) :below 0
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from 1 :to max-x
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    results))

(u:fn-> map/x (kernel function) list)
(defun map/x (kernel func)
  (declare (optimize speed))
  (let ((results)
        (max (max (max-x kernel)
                  (max-y kernel))))
    (loop :for x :from (- max) :to max
          :for cell = (select kernel x x)
          :when cell
            :do (push (funcall func cell) results))
    (loop :for x :from (- max) :to max
          :for cell = (select kernel (- x) x)
          :when cell
            :do (push (funcall func cell) results))
    results))

(u:fn-> map/h (kernel function) list)
(defun map/h (kernel func)
  (declare (optimize speed))
  (let ((results)
        (max (max-x kernel)))
    (loop :for x :from (- max) :to max
          :for cell = (select kernel x 0)
          :when cell
            :do (push (funcall func cell) results))
    results))

(u:fn-> map/v (kernel function) list)
(defun map/v (kernel func)
  (declare (optimize speed))
  (let ((results)
        (max (max-y kernel)))
    (loop :for y :from (- max) :to max
          :for cell = (select kernel 0 y)
          :when cell
            :do (push (funcall func cell) results))
    results))

(u:fn-> map (kernel function) list)
(defun map (kernel func)
  (declare (optimize speed))
  (values (funcall (mapper kernel) kernel func)))

(u:fn-> layout (keyword &optional u:ub16 u:ub16 u:ub16 u:ub16) function)
(defun layout (shape &optional (min-x 0) (max-x 1) (min-y 0) (max-y 1))
  (declare (optimize speed))
  (ecase shape
    (:rect
     (make-kernel-factory #'select/rect #'map/rect min-x max-x min-y max-y))
    (:+
     (make-kernel-factory #'select/+ #'map/+ min-x max-x min-y max-y))
    (:x
     (make-kernel-factory #'select/x #'map/x min-x max-x min-y max-y))
    (:ellipse
     (make-kernel-factory #'select/ellipse #'map/rect min-x max-x min-y max-y))
    (:h
     (make-kernel-factory #'select/h #'map/h min-x max-x min-y max-y))
    (:v
     (make-kernel-factory #'select/v #'map/v min-x max-x min-y max-y))
    (:.
     (make-kernel-factory #'select/rect #'map/rect 0 0 0 0))))

(u:fn-> ensure-layout ((or keyword function)) function)
(defun ensure-layout (layout)
  (declare (optimize speed))
  (etypecase layout
    (keyword (layout layout))
    (function layout)))

(u:fn-> detect (kernel function) (or tg:cell null))
(defun detect (kernel func)
  (declare (optimize speed))
  (block nil
    (map
     kernel
     (lambda (x)
       (u:when-let ((value (funcall func x)))
         (locally (declare (optimize (safety 0)))
           (return value)))))
    nil))

(u:fn-> filter (kernel function) list)
(defun filter (kernel func)
  (declare (optimize speed))
  (remove
   nil
   (map
    kernel
    (lambda (x)
      (when (funcall func x)
        x)))))

(u:fn-> count (kernel function) fixnum)
(defun count (kernel func)
  (declare (optimize speed))
  (list-length (filter kernel func)))

(u:fn-> convolve (tg:grid (or function keyword) function function) null)
(defun convolve (grid layout func test)
  (declare (optimize speed))
  (let ((layout (ensure-layout layout)))
    (dotimes (y (tg:height grid))
      (dotimes (x (tg:width grid))
        (let ((kernel (funcall layout grid x y)))
          (when (funcall test kernel)
            (funcall func kernel)))))))

(u:fn-> find (tg:grid (or function keyword) function) list)
(defun find (grid layout test)
  (declare (optimize speed))
  (let ((layout (ensure-layout layout))
        (items))
    (convolve grid layout (lambda (x) (push x items)) test)
    items))

(defun process (grid layout processor
                &key items (test (constantly t)) (generator #'identity))
  (declare (optimize speed)
           (function processor test generator))
  (let ((items (or items (find grid layout test))))
    (u:while items
      (let ((kernel (funcall generator (pop items))))
        (when (funcall test kernel)
          (u:when-let ((new (funcall processor kernel)))
            (push new items)))))))

(u:fn-> origin (kernel) (or tg:cell null))
(defun origin (kernel)
  (declare (optimize speed))
  (select kernel 0 0))

(u:fn-> cell->kernel (tg:grid tg:cell function) kernel)
(defun cell->kernel (grid cell layout)
  (declare (optimize speed))
  (values (funcall layout grid (tg:x cell) (tg:y cell))))
