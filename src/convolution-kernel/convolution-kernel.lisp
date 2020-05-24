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
   #:flood-fill
   #:find
   #:make-kernel
   #:map
   #:origin
   #:process
   #:reshape
   #:resolve))

(in-package #:net.mfiano.lisp.algae.convolution-kernel)

(defvar *shapes* (u:dict #'eq))

(defstruct (kernel
            (:constructor %make-kernel
                (shape grid x y mapper min-x max-x min-y max-y))
            (:conc-name nil)
            (:copier nil))
  (shape :rect :type keyword)
  (grid (tg::%make-grid) :type tg:grid)
  (x 0 :type u:ub32)
  (y 0 :type u:ub32)
  (min-x 0 :type u:ub32)
  (max-x 1 :type u:ub32)
  (min-y 0 :type u:ub32)
  (max-y 1 :type u:ub32)
  (mapper (constantly nil) :type function))

(u:fn-> make-kernel
        (tg:grid keyword &key
                 (:x u:ub32) (:y u:ub32) (:min-x u:ub32) (:max-x u:ub32)
                 (:min-y u:ub32) (:max-y u:ub32))
        kernel)
(defun make-kernel (grid shape
                    &key (x 0) (y 0) (min-x 0) (max-x 1) (min-y 0) (max-y 1))
  (declare (optimize speed))
  (let ((func (u:href *shapes* shape)))
    (declare (function func))
    (values (funcall func grid x y min-x max-x min-y max-y))))

(u:fn-> reshape (kernel keyword) kernel)
(defun reshape (kernel shape)
  (declare (optimize speed))
  (make-kernel (grid kernel)
               shape
               :x (x kernel)
               :y (y kernel)
               :min-x (min-x kernel)
               :max-x (max-x kernel)
               :min-y (min-y kernel)
               :max-y (max-y kernel)))

(defun register-shape (name mapper)
  (setf (u:href *shapes* name)
        (lambda (grid x y min-x max-x min-y max-y)
          (%make-kernel name grid x y mapper min-x max-x min-y max-y))))

(defmacro define-shape (shape () &body body)
  (let ((do-name (u:symbolicate '#:do/ shape))
        (map-name (u:symbolicate '#:map/ shape)))
    `(progn
       (defmacro ,do-name ((kernel cell) &body body)
         (u:with-gensyms (grid ox oy)
           `(let ((,grid (grid ,kernel))
                  (,ox (x ,kernel))
                  (,oy (y ,kernel)))
              ,,@body)))
       (u:fn-> ,map-name (kernel function) list)
       (defun ,map-name (kernel func)
         (declare (optimize speed))
         (let (results)
           (,do-name (kernel cell)
             (u:when-let ((value (funcall func cell)))
               (push value results)))
           results))
       (register-shape ,shape #',map-name))))

(defmacro default-mapper (test)
  `(u:with-gensyms (max-x max-y x y)
     `(loop :with ,max-x = (max-x ,kernel)
            :with ,max-y = (max-y ,kernel)
            :for ,y :of-type u:b32 :from ,max-y :downto (- ,max-y)
            :do (loop :for ,x :of-type u:b32 :from (- ,max-x) :to ,max-x
                      :for ,cell = (when ,,test
                                     (%resolve ,grid ,ox ,oy ,x ,y))
                      :when ,cell
                        :do (progn ,@body)))))

(defmacro %resolve (grid ox oy x y)
  `(tg:get-cell ,grid (+ ,ox ,x) (+ ,oy ,y)))

(defmacro do-kernel ((kernel cell) &body body)
  (u:once-only (kernel)
    `(case (shape ,kernel)
       (:rect (do/rect (,kernel ,cell) ,@body))
       (:+ (do/+ (,kernel ,cell) ,@body))
       (:x (do/x (,kernel ,cell) ,@body))
       (:ellipse (do/ellipse (,kernel ,cell) ,@body))
       (t (map ,kernel (lambda (,cell) ,@body))))))

(defmacro define-properties (&body body)
  (let ((properties (mapcar #'u:ensure-list body)))
    `(progn
       (tg:define-properties ,@(mapcar #'car properties))
       ,@(loop :for (name . options) :in properties
               :append (generate-property-functions name options)))))

(define-shape :rect ()
  (u:with-gensyms (max-x max-y x y)
    `(loop :with ,max-x = (max-x ,kernel)
           :with ,max-y = (max-y ,kernel)
           :for ,y :from ,max-y :downto (- ,max-y)
           :do (loop :for ,x :from (- ,max-x) :to ,max-x
                     :for ,cell = (%resolve ,grid ,ox ,oy ,x ,y)
                     :when ,cell
                       :do (progn ,@body)))))

(define-shape :ellipse ()
  (default-mapper
   `(<= (+ (/ (expt ,x 2) (expt (/ ,max-x 2) 2f0))
           (/ (expt ,y 2) (expt (/ ,max-y 2) 2f0)))
        1f0)))

(define-shape :+ ()
  (u:with-gensyms (max-x max-y x y)
    `(let ((,max-x (max-x ,kernel))
           (,max-y (max-y ,kernel)))
       (loop :for ,y :from (- ,max-y) :to ,max-y
             :for ,cell = (%resolve ,grid ,ox ,oy 0 ,y)
             :when ,cell
               :do (progn ,@body))
       (loop :for ,x :from (- ,max-x) :below 0
             :for ,cell = (%resolve ,grid ,ox ,oy ,x 0)
             :when ,cell
               :do (progn ,@body))
       (loop :for ,x :from 1 :to ,max-x
             :for ,cell = (%resolve ,grid ,ox ,oy ,x 0)
             :when ,cell
               :do (progn ,@body)))))

(define-shape :x ()
  (u:with-gensyms (max x cell1 cell2)
    `(loop :with ,max = (max (max-x ,kernel) (max-y ,kernel))
           :for ,x :from (- ,max) :to ,max
           :for ,cell1 = (%resolve ,grid ,ox ,oy ,x ,x)
           :for ,cell2 = (%resolve ,grid ,ox ,oy (- ,x) ,x)
           :when ,cell1
             :do (let ((,cell ,cell1)) ,@body)
           :when (and ,cell2 (not (zerop ,x)))
             :do (let ((,cell ,cell2)) ,@Body))))

(defun generate-property-functions (name options)
  (u:with-gensyms (object cell)
    (destructuring-bind (&key remove) options
      (let ((constant-name (u:symbolicate '#:+ name '#:+))
            (predicate-name (u:symbolicate name '#:-p))
            (adder-name (u:symbolicate '#:add- name))
            (remover-name (u:symbolicate '#:remove- name)))
        `((defun ,predicate-name (,object)
            (u:when-let ((,cell (if (kernel-p ,object)
                                    (origin ,object)
                                    ,object)))
              (tg:cell-contains-p ,cell ,constant-name)))
          (export ',predicate-name)
          (defun ,adder-name (,object)
            (u:when-let ((,cell (if (kernel-p ,object)
                                    (origin ,object)
                                    ,object)))
              ,@(when remove
                  `((tg:remove-properties
                     ,cell
                     ,@(mapcar (lambda (x) (u:symbolicate '#:+ x '#:+))
                        remove))))
              (tg:add-properties ,cell ,constant-name)))
          (export ',adder-name)
          (defun ,remover-name (,object)
            (u:when-let ((,cell (if (kernel-p ,object)
                                    (origin ,object)
                                    ,object)))
              (tg:remove-properties ,cell ,constant-name)))
          (export ',remover-name))))))

(u:fn-> resolve (kernel u:b32 u:b32) (or tg:cell null))
(declaim (inline resolve))
(defun resolve (kernel x y)
  (declare (optimize speed))
  (%resolve (grid kernel) (x kernel) (y kernel) x y))

(u:fn-> map (kernel function) list)
(defun map (kernel func)
  (declare (optimize speed))
  (values (funcall (mapper kernel) kernel func)))

(u:fn-> detect (kernel function
                        &optional (integer 1 #.most-positive-fixnum))
        boolean)
(defun detect (kernel func &optional (count 1))
  (declare (optimize speed))
  (let ((i 0))
    (declare (fixnum i))
    (do-kernel (kernel cell)
      (when (funcall func cell)
        (incf i)
        (when (>= i count)
          (locally (declare (optimize (safety 0)))
            (return-from detect t)))))
    nil))

(u:fn-> count (kernel function) fixnum)
(defun count (kernel func)
  (declare (optimize speed))
  (let ((count 0))
    (declare (fixnum count))
    (do-kernel (kernel cell)
      (when (funcall func cell)
        (incf count)))
    count))

(u:fn-> convolve (kernel function &optional function) null)
(defun convolve (kernel func &optional test)
  (declare (optimize speed))
  (let ((grid (grid kernel)))
    (dotimes (y (tg:height grid))
      (dotimes (x (tg:width grid))
        (setf (x kernel) x
              (y kernel) y)
        (when (or (null test) (funcall test kernel))
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
(declaim (inline origin))
(defun origin (kernel)
  (declare (optimize speed))
  (%resolve (grid kernel) (x kernel) (y kernel) 0 0))

(u:fn-> align (kernel tg:cell) kernel)
(defun align (kernel cell)
  (declare (optimize speed))
  (setf (x kernel) (tg:x cell)
        (y kernel) (tg:y cell))
  kernel)

(u:fn-> flood-fill (kernel tg:cell function (simple-array bit (* *)))
        hash-table)
(defun flood-fill (kernel cell test visited)
  (declare (optimize speed)
           (function test))
  (let ((cells (u:dict #'eq))
        (work-list nil))
    (flet ((%test-cell (cell)
             (when (funcall test cell)
               (setf (aref visited (tg:x cell) (tg:y cell)) 1)
               (do-kernel ((align kernel cell) neighbor)
                 (let ((x (tg:x neighbor))
                       (y (tg:y neighbor)))
                   (when (zerop (aref visited x y))
                     (setf (aref visited x y) 1)
                     (push neighbor work-list))))
               (setf (u:href cells cell) cell)
               nil)))
      (%test-cell cell)
      (loop :for x = (pop work-list)
            :while x
            :do (%test-cell x))
      cells)))
