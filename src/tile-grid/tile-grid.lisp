(in-package #:cl-user)

;;;; A simple tile grid implementation.
;;;; Presents a grid object which has a width and height, and a 2-dimensional
;;;; array of cells. Cells have X and Y coordinates, as well as a bitfield of
;;;; various user-definable properties that can be added/removed with an
;;;; included API.

(defpackage #:algae.tile-grid
  (:local-nicknames
   (#:u #:golden-utils))
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

(in-package #:algae.tile-grid)

(defstruct (cell
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
(u:defun-inline get-cell (grid x y)
  (declare (optimize speed))
  (let ((width (width grid))
        (height (height grid)))
    (when (and (<= 0 x)
               (< x width)
               (<= 0 y)
               (< y height))
      (locally (declare (optimize (safety 0)))
        (aref (cells grid) (+ (* y width) x))))))

(u:fn-> reset-grid (grid) grid)
(defun reset-grid (grid)
  (declare (optimize speed))
  (do-cells () grid cell
    (setf (value cell) 0))
  grid)

(u:fn-> copy-grid (grid grid) grid)
(defun copy-grid (source target)
  (setf (width target) (width source)
        (height target) (height source)
        (cells target) (cells source))
  target)

(defun clear-properties (cell)
  (setf (value cell) 0))

(defgeneric add-properties (cell &rest properties)
  (:method (cell &rest properties)
    (let ((mask (apply #'logior properties)))
      (setf (value cell) (logior (value cell) mask)))))

(defgeneric remove-properties (cell &rest properties)
  (:method (cell &rest properties)
    (let ((mask (apply #'logior properties)))
      (setf (value cell) (logand (value cell) (lognot mask))))))

(defgeneric cell-contains-p (cell &rest properties)
  (:method (cell &rest properties)
    (let ((mask (apply #'logior properties)))
      (and cell (= mask (logand (value cell) mask))))))

(u:fn-> cell-empty-p ((or cell null)) boolean)
(u:defun-inline cell-empty-p (cell)
  (declare (optimize speed))
  (or (null cell) (zerop (value cell))))

(u:fn-> copy-properties (cell cell) cell)
(defun copy-properties (source target)
  (declare (optimize speed))
  (setf (value target) (value source))
  target)

(defun check-property-count (property-names)
  (let ((max (integer-length most-positive-fixnum)))
    (when (> (length property-names) max)
      `((error "Only up to ~s properties are allowed." ,max)))))

(defmacro define-properties (&body body)
  `(progn
     ,@(check-property-count body)
     ,@(loop :for name :in body
             :for property = (u:symbolicate '#:+ name '#:+)
             :for i :from 0
             :append `((u:define-constant ,property ,(ash 1 i))))))
