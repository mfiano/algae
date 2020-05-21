(in-package #:cl-user)

;;;; A simple tile  grid implementation.
;;;; Presents a grid object which has a width and height, and a 2-dimensional
;;;; array of cells. Cells have X and Y coordinates, as well as a bitfield of
;;;; various user-definable properties that can be added/removed with an
;;;; included API.

(defpackage #:net.mfiano.lisp.algae.tile-grid
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:export
   #:add-properties
   #:cell
   #:cell-contains-p
   #:cell-empty-p
   #:clear-properties
   #:copy-grid
   #:copy-properties
   #:define-properties
   #:do-cells
   #:get-cell
   #:grid
   #:height
   #:make-grid
   #:remove-properties
   #:reset-grid
   #:width
   #:x
   #:y))

(in-package #:net.mfiano.lisp.algae.tile-grid)

(deftype cell () '(simple-array fixnum (3)))

(defstruct (cell
            (:type (vector fixnum))
            (:constructor make-cell (x y))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (value 0 :type fixnum))

(defstruct (grid
            (:constructor %make-grid)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (cells (make-array 0 :element-type 'cell)
   :type (simple-array cell (*))))

(defmacro do-cells ((&key (w 'w) (h 'h) (x1 0) (y1 0) (x2 w) (y2 h)) grid cell
                    &body body)
  (u:with-gensyms (grid-sym x y)
    (destructuring-bind (cell &optional (x x) (y y)) (u:ensure-list cell)
      `(loop :with ,grid-sym = ,grid
             :with ,w = (width ,grid-sym)
             :with ,h = (height ,grid-sym)
             :for ,y :from ,y1 :below ,y2
             :do (loop :for ,x :from ,x1 :below ,x2
                       :for ,cell = (get-cell ,grid-sym ,x ,y)
                       :do (progn ,@body))))))

(u:fn-> make-grid (u:ub16 u:ub16) grid)
(defun make-grid (width height)
  (declare (optimize speed))
  (let* ((cells (make-array (* width height)))
         (grid (%make-grid :width width :height height :cells cells)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref cells (+ (* y width) x)) (make-cell x y))))
    grid))

(u:fn-> get-cell (grid fixnum fixnum) (or cell null))
(defun get-cell (grid x y)
  (declare (optimize speed))
  (when (and (<= 0 x (1- (width grid)))
             (<= 0 y (1- (height grid))))
    (aref (cells grid) (+ (* y (width grid)) x))))

(u:fn-> reset-grid (grid) grid)
(defun reset-grid (grid)
  (declare (optimize speed))
  (do-cells () grid cell
    (setf (value cell) 0))
  grid)

(defun clear-properties (cell)
  (setf (value cell) 0))

(u:fn-> add-properties (cell &rest fixnum) cell)
(defun add-properties (cell &rest properties)
  (declare (optimize speed))
  (let ((mask (apply #'logior properties)))
    (declare (fixnum mask))
    (setf (value cell) (logior (value cell) mask))
    cell))

(define-compiler-macro add-properties (&whole whole cell &rest properties)
  (u:with-gensyms (cell-sym)
    (if (every #'constantp properties)
        `(let ((,cell-sym ,cell))
           (setf (value ,cell-sym) (logior (value ,cell-sym)
                                           ,(apply #'logior properties)))
           ,cell-sym)
        whole)))

(u:fn-> remove-properties (cell &rest fixnum) cell)
(defun remove-properties (cell &rest properties)
  (declare (optimize speed))
  (let ((mask (apply #'logior properties)))
    (declare (fixnum mask))
    (setf (value cell) (logand (value cell) (lognot mask)))
    cell))

(define-compiler-macro remove-properties (&whole whole cell &rest properties)
  (u:with-gensyms (cell-sym)
    (if (every #'constantp properties)
        `(let ((,cell-sym ,cell))
           (setf (value ,cell-sym)
                 (logand (value ,cell-sym)
                         ,(lognot (apply #'logior properties))))
           ,cell-sym)
        whole)))

(u:fn-> cell-contains-p (cell &rest fixnum) boolean)
(defun cell-contains-p (cell &rest properties)
  (declare (optimize speed))
  (let ((mask (apply #'logior properties)))
    (declare (fixnum mask))
    (= mask (logand (value cell) mask))))

(define-compiler-macro cell-contains-p (&whole whole cell &rest properties)
  (if (every #'constantp properties)
      (let ((mask (apply #'logior properties)))
        `(= ,mask (logand (value ,cell) ,mask)))
      whole))

(u:fn-> cell-empty-p (cell) boolean)
(defun cell-empty-p (cell)
  (declare (optimize speed))
  (zerop (value cell)))

(u:fn-> copy-properties (cell cell) cell)
(defun copy-properties (source target)
  (declare (optimize speed))
  (setf (value target) (value source))
  target)

(defmacro define-properties (&body body)
  (let ((max (integer-length most-positive-fixnum)))
    `(progn
       ,@(unless (every
                 (lambda (x)
                   (let ((name (symbol-name x)))
                     (char= #\+ (char name 0) (char name (1- (length name))))))
                 body)
           `((error "Property names must be enclosed in + characters since ~
                     they are constants.")))
       ,@(when (> (length body) max)
           `((error "Only up to ~s properties are allowed." ,max)))
       ,@(loop :for property :in body
               :for i :from 0
               :collect `(u:define-constant ,property ,(ash 1 i))))))
