(in-package #:cl-user)

;;;; A simple tile grid implementation.
;;;; Presents a grid object which has a width and height, and a 2-dimensional
;;;; array of cells. Cells have X and Y coordinates, as well as a bitfield of
;;;; various user-definable properties that can be added/removed with an
;;;; included API.

(defpackage #:algae.tile-grid
  (:local-nicknames
   (#:u #:mfiano-utils))
  (:use #:cl)
  (:export
   #:add-properties
   #:cell
   #:cell-contains-p
   #:cell-empty-p
   #:clear-properties
   #:copy-grid
   #:do-cells
   #:get-cell
   #:grid
   #:height
   #:make-grid
   #:properties
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
  (properties (u:dict #'eq) :type hash-table))

(defstruct (grid
            (:constructor %make-grid)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (cells (make-array 0 :element-type 'cell) :type (simple-array cell (*))))

(defmacro do-cells ((&key (w 'w) (h 'h) (x1 0) (y1 0) (x2 w) (y2 h)) grid cell &body body)
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
    (clear-properties cell))
  grid)

(u:fn-> copy-grid (grid grid) grid)
(defun copy-grid (source target)
  (setf (width target) (width source)
        (height target) (height source)
        (cells target) (cells source))
  target)

(defun clear-properties (cell)
  (clrhash (properties cell)))

(defgeneric add-properties (cell type &rest properties)
  (:method (cell type &rest properties)
    (let ((cell-properties (properties cell)))
      (unless (u:href cell-properties type)
        (setf (u:href cell-properties type) (u:dict #'equalp)))
      (dolist (property properties)
        (setf (u:href cell-properties type property) property)))))

(defgeneric remove-properties (cell type &rest properties)
  (:method (cell type &rest properties)
    (let ((cell-properties (properties cell)))
      (when (u:href cell-properties type)
        (dolist (property properties)
          (remhash property (u:href cell-properties type)))
        (when (zerop (hash-table-count (u:href cell-properties type)))
          (remhash type cell-properties))))))

(defgeneric cell-contains-p (cell type property)
  (:method (cell type property)
    (when cell
      (let ((cell-properties (properties cell)))
        (u:when-let ((type-table (u:href cell-properties type)))
          (when (u:href type-table property)
            t))))))

(u:fn-> cell-empty-p ((or cell null)) boolean)
(u:defun-inline cell-empty-p (cell)
  (declare (optimize speed))
  (or (null cell) (zerop (hash-table-count (properties cell)))))
