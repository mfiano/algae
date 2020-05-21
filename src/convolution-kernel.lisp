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
   #:align
   #:convolve
   #:count
   #:define-properties
   #:detect
   #:filter
   #:find
   #:make-kernel
   #:map
   #:origin
   #:process
   #:reshape
   #:select))

(in-package #:net.mfiano.lisp.algae.convolution-kernel)

(defstruct (kernel
            (:constructor %%make-kernel)
            (:conc-name nil)
            (:copier nil))
  (grid (tg::%make-grid) :type tg:grid)
  (origin-x 0 :type u:ub32)
  (origin-y 0 :type u:ub32)
  (min-x 0 :type u:ub32)
  (max-x 1 :type u:ub32)
  (min-y 0 :type u:ub32)
  (max-y 1 :type u:ub32)
  (selector (constantly nil) :type function)
  (mapper (constantly nil) :type function))

(u:fn-> %make-kernel (tg:grid function function u:ub32 u:ub32 u:ub32 u:ub32
                              u:ub32 u:ub32)
        kernel)
(defun %make-kernel (grid selector mapper x y min-x max-x min-y max-y)
  (%%make-kernel :grid grid
                 :origin-x x
                 :origin-y y
                 :min-x min-x
                 :max-x max-x
                 :min-y min-y
                 :max-y max-y
                 :selector selector
                 :mapper mapper))

(u:fn-> make-kernel (tg:grid keyword
                             &key
                             (:x u:ub32)
                             (:y u:ub32)
                             (:min-x u:ub32)
                             (:max-x u:ub32)
                             (:min-y u:ub32)
                             (:max-y u:ub32))
        kernel)
(defun make-kernel (grid shape
                    &key (x 0) (y 0) (min-x 0) (max-x 1) (min-y 0) (max-y 1))
  (declare (optimize speed))
  (ecase shape
    (:rect
     (%make-kernel grid #'select/rect #'map/rect x y min-x max-x min-y max-y))
    (:+
     (%make-kernel grid #'select/+ #'map/+ x y min-x max-x min-y max-y))
    (:x
     (%make-kernel grid #'select/x #'map/x x y min-x max-x min-y max-y))
    (:ellipse
     (%make-kernel grid #'select/ellipse #'map/rect x y min-x max-x min-y
                   max-y))
    (:h
     (%make-kernel grid #'select/h #'map/h x y min-x max-x min-y max-y))
    (:v
     (%make-kernel grid #'select/v #'map/v x y min-x max-x min-y max-y))
    (:.
     (%make-kernel grid #'select/rect #'map/rect x y 0 0 0 0))))

(u:fn-> reshape (kernel keyword) kernel)
(defun reshape (kernel shape)
  (declare (optimize speed))
  (make-kernel (grid kernel)
               shape
               :x (origin-x kernel)
               :y (origin-y kernel)
               :min-x (min-x kernel)
               :max-x (max-x kernel)
               :min-y (min-y kernel)
               :max-y (max-y kernel)))

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

(u:fn-> detect (kernel function) boolean)
(defun detect (kernel func)
  (declare (optimize speed))
  (block nil
    (map
     kernel
     (lambda (x)
       (when (funcall func x)
         (locally (declare (optimize (safety 0)))
           (return t)))))
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

(u:fn-> convolve (kernel function function) null)
(defun convolve (kernel func test)
  (declare (optimize speed))
  (let ((grid (grid kernel)))
    (dotimes (y (tg:height grid))
      (dotimes (x (tg:width grid))
        (setf (origin-x kernel) x
              (origin-y kernel) y)
        (when (funcall test kernel)
          (funcall func kernel))))))

(u:fn-> find (kernel function) list)
(defun find (kernel test)
  (declare (optimize speed))
  (let ((items))
    (convolve kernel (lambda (x) (push x items)) test)
    items))

(defun process (kernel processor
                &key items (test (constantly t)) (generator #'identity))
  (declare (optimize speed)
           (function processor test generator))
  (let ((items (or items (find kernel test))))
    (u:while items
      (let ((kernel (funcall generator (pop items))))
        (when (funcall test kernel)
          (u:when-let ((new (funcall processor kernel)))
            (push new items)))))))

(u:fn-> origin (kernel) (or tg:cell null))
(defun origin (kernel)
  (declare (optimize speed))
  (select kernel 0 0))

(u:fn-> align (kernel tg:cell) kernel)
(defun align (kernel cell)
  (declare (optimize speed))
  (setf (origin-x kernel) (tg:x cell)
        (origin-y kernel) (tg:y cell))
  kernel)
  (declare (optimize speed))
  (values (funcall layout grid (tg:x cell) (tg:y cell))))

(defun generate-property-functions (name options)
  (u:with-gensyms (object cell)
    (destructuring-bind (&key remove) options
      (let ((constant-name (u:symbolicate '#:+ name '#:+))
            (predicate-name (u:symbolicate name '#:-p))
            (adder-name (u:symbolicate '#:add- name))
            (remover-name (u:symbolicate '#:remove- name)))
        `((defun ,predicate-name (,object)
            (let ((,cell (if (kernel-p ,object) (origin ,object) ,object)))
              (tg:cell-contains-p ,cell ,constant-name)))
          (export ',predicate-name)
          (defun ,adder-name (,object)
            (let ((,cell (if (kernel-p ,object) (origin ,object) ,object)))
              ,@(when remove
                  `((tg:remove-properties
                     ,cell
                     ,@(mapcar (lambda (x) (u:symbolicate '#:+ x '#:+))
                        remove))))
              (tg:add-properties ,cell ,constant-name)))
          (export ',adder-name)
          (defun ,remover-name (,object)
            (let ((,cell (if (kernel-p ,object) (origin ,object) ,object)))
              (tg:remove-properties ,cell ,constant-name)))
          (export ',remover-name))))))

(defmacro define-properties (&body body)
  (let ((properties (mapcar #'u:ensure-list body)))
    `(progn
       (tg:define-properties ,@(mapcar #'car properties))
       ,@(loop :for (name . options) :in properties
               :append (generate-property-functions name options)))))
