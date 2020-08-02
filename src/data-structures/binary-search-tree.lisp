(in-package #:cl-user)

;;;; An implementation of the binary search tree data structure.

(defpackage #:net.mfiano.lisp.algae.data-structures.binary-search-tree
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils))
  (:use #:cl)
  (:shadow
   #:delete
   #:find
   #:max
   #:min)
  (:export
   #:delete
   #:find
   #:insert
   #:make-tree
   #:max
   #:min
   #:next
   #:node
   #:previous
   #:tree
   #:valid-p
   #:walk))

(in-package #:net.mfiano.lisp.algae.data-structures.binary-search-tree)

(defstruct (tree
            (:constructor %make-tree)
            (:predicate nil)
            (:copier nil))
  sentinel
  root
  item-type
  (key #'identity :type function)
  (sorter #'< :type function)
  (hash-test #'eql :type function))

(defstruct (node
            (:constructor %make-node)
            (:predicate nil)
            (:copier nil))
  tree
  key
  data
  parent
  left
  right)

(u:define-printer (tree stream :identity t :type nil)
  (format stream "TREE"))

(u:define-printer (node stream :identity t :type nil)
  (format stream "NODE"))

(u:defun-inline node-p (node)
  (declare (optimize speed))
  (unless (and node (eq node (tree-sentinel (node-tree node))))
    node))

(u:fn-> make-node (tree t) node)
(defun make-node (tree item)
  (declare (optimize speed))
  (let ((sentinel (tree-sentinel tree))
        (node (%make-node :tree tree
                          :key (when item (funcall (tree-key tree) item))
                          :data (u:dict (tree-hash-test tree) item item))))
    (setf (node-left node) sentinel
          (node-right node) sentinel)
    node))

(u:fn-> make-tree (&key (:item-type symbol)
                        (:key function)
                        (:sort function)
                        (:hash-test function))
        tree)
(defun make-tree (&key item-type (key #'identity) (sort #'<) (hash-test #'eql))
  (declare (optimize speed))
  (unless item-type
    (error "Must specify :ITEM-TYPE denoting the type of items stored in the ~
            tree."))
  (let* ((tree (%make-tree :item-type item-type
                           :key key
                           :sorter sort
                           :hash-test hash-test))
         (sentinel (make-node tree nil)))
    (setf (tree-sentinel tree) sentinel
          (node-left sentinel) sentinel
          (node-right sentinel) sentinel
          (tree-root tree) sentinel)
    (clrhash (node-data (tree-sentinel tree)))
    tree))

(u:fn-> valid-p (tree) boolean)
(defun valid-p (tree)
  (declare (optimize speed))
  (let ((previous nil))
    (labels ((%check (node sorter)
               (declare (function sorter))
               (when (node-p node)
                 (when (or (null (%check (node-left node) sorter))
                           (and previous
                                (funcall sorter
                                         (node-key node)
                                         (node-key previous))))
                   (return-from %check))
                 (setf previous node)
                 (return-from %check
                   (%check (node-right node) sorter)))
               t))
      (%check (tree-root tree) (tree-sorter tree)))))

(u:fn-> %walk/pre-order (node function) null)
(defun %walk/pre-order (node func)
  (declare (optimize speed))
  (when (node-p node)
    (u:do-hash-keys (k (node-data node))
      (funcall func k))
    (%walk/pre-order (node-left node) func)
    (%walk/pre-order (node-right node) func)))

(u:fn-> %walk/in-order (node function) null)
(defun %walk/in-order (node func)
  (declare (optimize speed))
  (when (node-p node)
    (loop :with current = node
          :with stack
          :do (cond
                ((node-p current)
                 (push current stack)
                 (setf current (node-left current)))
                (stack
                 (setf current (pop stack))
                 (u:do-hash-keys (k (node-data current))
                   (funcall func k))
                 (setf current (node-right current)))
                (t (loop-finish))))))

(u:fn-> %walk/post-order (node function) null)
(defun %walk/post-order (node func)
  (declare (optimize speed))
  (when (node-p node)
    (%walk/post-order (node-left node) func)
    (%walk/post-order (node-right node) func)
    (u:do-hash-keys (k (node-data node))
      (funcall func k))))

(u:fn-> walk (tree function &key (:order keyword)) null)
(defun walk (tree func &key (order :in))
  (declare (optimize speed))
  (let ((node (tree-root tree)))
    (ecase order
      (:pre (%walk/pre-order node func))
      (:in (%walk/in-order node func))
      (:post (%walk/post-order node func)))))

(u:fn-> transplant (node node) node)
(u:defun-inline transplant (node1 node2)
  (declare (optimize speed))
  (let ((parent (node-parent node1)))
    (cond
      ((not (node-p parent))
       (setf (tree-root (node-tree node1)) node2))
      ((eq node1 (node-left parent))
       (setf (node-left parent) node2))
      (t (setf (node-right parent) node2)))
    (setf (node-parent node2) (node-parent node1))))

(u:fn-> insert (tree t) node)
(defun insert (tree item)
  (declare (optimize speed))
  (u:if-let ((node (node-p (nth-value 1 (find tree item)))))
    (prog1 node
      (setf (u:href (node-data node) item) item))
    (let ((node (make-node tree item)))
      (loop :with sorter = (tree-sorter tree)
            :with key = (node-key node)
            :with current = (tree-root tree)
            :with parent = (tree-sentinel tree)
            :while (node-p current)
            :do (setf parent current)
                (if (funcall sorter key (node-key current))
                    (setf current (node-left current))
                    (setf current (node-right current)))
            :finally (setf (node-parent node) parent)
                     (cond
                       ((not (node-p parent))
                        (setf (tree-root tree) node))
                       ((funcall sorter key (node-key parent))
                        (setf (node-left parent) node))
                       (t (setf (node-right parent) node))))
      node)))

(u:fn-> delete (tree t) (or node null))
(defun delete (tree item)
  (declare (optimize speed))
  (labels ((%delete (node)
             (cond
               ((not (node-p (node-left node)))
                (transplant node (node-right node)))
               ((not (node-p (node-right node)))
                (transplant node (node-left node)))
               (t (let* ((successor (min (node-right node)))
                         (left (node-left successor))
                         (right (node-right successor)))
                    (unless (eq node (node-parent successor))
                      (transplant successor right)
                      (setf (node-right successor) (node-right node)
                            (node-parent right) successor))
                    (transplant node successor)
                    (setf (node-left successor) (node-left node))
                    (when (node-p left)
                      (setf (node-parent left) successor)))))))
    (u:when-let ((node (node-p (nth-value 1 (find tree item))))
                 (sentinel (tree-sentinel tree)))
      (when (u:href (node-data node) item)
        (if (<= (hash-table-count (node-data node)) 1)
            (progn
              (u:when-let ((new-root (%delete node)))
                (setf (tree-root tree) new-root))
              (setf (node-parent sentinel) sentinel)
              (clrhash (node-data node)))
            (remhash item (node-data node))))
      node)))

(u:fn-> min (node) (or node null))
(defun min (node)
  (declare (optimize speed))
  (when (node-p node)
    (loop :with current = node
          :for left = (node-left node)
          :while (node-p left)
          :do (setf current left)
          :finally (return current))))

(u:fn-> max (node) (or node null))
(defun max (node)
  (declare (optimize speed))
  (when (node-p node)
    (loop :with current = node
          :for right = (node-right node)
          :while (node-p right)
          :do (setf current right)
          :finally (return current))))

(u:fn-> previous (node) (or node null))
(defun previous (node)
  (declare (optimize speed))
  (when (node-p node)
    (u:if-let ((left (node-p (node-left node))))
      (max left)
      (loop :for current = node :then parent
            :for parent = (node-parent current)
            :while (and (node-p parent)
                        (eq current (node-left parent)))
            :finally (return parent)))))

(u:fn-> next (node) (or node null))
(defun next (node)
  (declare (optimize speed))
  (when (node-p node)
    (u:if-let ((right (node-p (node-right node))))
      (min right)
      (loop :for current = node :then parent
            :for parent = (node-parent current)
            :while (and (node-p parent)
                        (eq current (node-right parent)))
            :finally (return parent)))))

(u:fn-> find (tree t) (values &optional t node))
(defun find (tree item)
  (declare (optimize speed))
  (labels ((%find (node key sorter)
             (declare (function sorter))
             (u:when-let ((result (and (node-p node) (node-key node))))
               (cond
                 ((funcall sorter key result)
                  (%find (node-left node) key sorter))
                 ((funcall sorter result key)
                  (%find (node-right node) key sorter))
                 (t node)))))
    (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (when (typep item (tree-item-type tree))
        (u:when-let ((node (%find (tree-root tree)
                                  (funcall (tree-key tree) item)
                                  (tree-sorter tree))))
          (values (u:href (node-data node) item)
                  node))))))
