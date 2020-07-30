(in-package #:cl-user)

;;;; An implementation of the doubly linked list data structure.

(defpackage #:net.mfiano.lisp.algae.data-structures.doubly-linked-list
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:length
   #:list
   #:make-list)
  (:export
   #:delete
   #:find
   #:head
   #:insert
   #:length
   #:list
   #:list-values
   #:make-list
   #:next
   #:node
   #:previous
   #:tail
   #:value))

(in-package #:net.mfiano.lisp.algae.data-structures.doubly-linked-list)

(defstruct (list
            (:constructor %make-list)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  "A doubly linked list that holds sequential nodes which have links to their
  previous and next node."
  head
  tail
  (%length 0 :type fixnum))

(defstruct (node
            (:conc-name nil)
            (:copier nil))
  "A doubly linked list node with references to its previous and next node."
  value
  previous
  next)

(defun list-values (list)
  "Convert the doubly-linked list `LIST` into a Lisp list of its nodes' values."
  (loop :for node = (head list) :then (next node)
        :while node
        :collect (value node)))

(u:define-printer (list stream :type nil)
  (format stream "~s" (list-values list)))

(u:define-printer (node stream)
  (format stream "~s" (value node)))

(defun make-list (&rest values)
  "Create a new doubly linked list, optionally pre-populated with `VALUES`. "
  (loop :with list = (%make-list)
        :for value :in values
        :for node = (insert list value)
          :then (insert list value :target node)
        :finally (return list)))

(u:defun-inline length (list)
  "Return the number of elements in the doubly linked list `LIST`."
  (%length list))

(defun find (list value &key start end (key #'identity) (test #'eql) from-end)
  "Search for a node with the given `VALUE` in the doubly linked list `LIST`.

`START` and `END` are NODE objects to begin and end searching, inclusively.

`KEY` specifies a function that is called with `VALUE` as its only argument. It
defaults to `#'IDENTITY`.

`TEST` specifies a function used to compare the`VALUE` as it traverses nodes in
the list. It defaults to `#'EQL`.

`FROM-END`, when specified, traverses the list in reverse, from tail to head."
  (declare (function key test))
  (loop :for node = (or start (if from-end (tail list) (head list)))
          :then (if from-end (previous node) (next node))
        :while node
        :when (funcall test
                       (funcall key (value node))
                       (funcall key value))
          :do (return node)
        :when (eq node end)
          :do (return)))

(defun insert (list value &key target (where :after))
  "Insert a new node into the doubly linked list `LIST`, constructed to hold
`VALUE`.

`TARGET` is an existing node to place the new node adjacent to. If `TARGET` is
NIL, the head is implicitly targetted.

`WHERE` can be either `:BEFORE` or `:AFTER`, and specifies on which side of the
target node to insert the new node. If unspecified, defaults to `:AFTER`."
  (flet ((%insert (list previous next value)
           (let ((node (make-node :value value :previous previous :next next)))
             (if previous
                 (setf (next previous) node)
                 (setf (head list) node))
             (if next
                 (setf (previous next) node)
                 (setf (tail list) node))
             (incf (%length list))
             node)))
    (let ((target (or target (head list))))
      (ecase where
        (:before (%insert list (when target (previous target)) target value))
        (:after (%insert list target (when target (next target)) value))))))

(defun delete (list object &key (key #'identity) (test #'eql) from-end)
  "Delete an object from the doubly linked list `LIST`.

`OBJECT` can be either a NODE object, or some value that is stored in the value
of a NODE object. In the case of a non-NODE value, the list is searched
linearly for a matching node before deletion occurs. Returns two values, the
modified list, and a boolean specifying if a node was deleted.

`KEY` specifies a function that is called with `OBJECT` as its only argument, if
`OBJECT` is not of type node. It defaults to `#'IDENTITY`. This argument has no
effect if `OBJECT` is of type NODE.

`TEST` specifies a function used to compare the value of `OBJECT` as it
traverses nodes in the list. It defaults to `#'EQL`. This argument has no effect
if `OBJECT` is of type NODE.

Note: As specified above, this function traverses the list linearly for an
object to delete if `OBJECT` is not a NODE."
  (flet ((%delete (list node)
           (let ((previous (when node (previous node)))
                 (next (when node (next node))))
             (if previous
                 (setf (next previous) next)
                 (setf (head list) next))
             (if next
                 (setf (previous next) previous)
                 (setf (tail list) previous))
             (decf (%length list))
             (values list t))))
    (if (node-p object)
        (%delete list object)
        (u:if-let ((node (find list
                               object
                               :key key
                               :test test
                               :from-end from-end)))
          (%delete list node)
          (values list nil)))))
