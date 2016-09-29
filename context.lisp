(in-package :pile)

(defstruct pile-element)

(defstruct pile-semantic-element
  (name (error "semantic element must have a name") :type keyword))

(defstruct (pile-site-element (:include pile-semantic-element)))

(define-constant +null-element+
    (or (when (boundp '+null-element+)
          (symbol-value '+null-element+))
        (make-pile-element)))

(defstruct (pile-nk-ptr-element (:include pile-element))
  (ptr (error "pointer to nk element must be provided")
       :type cffi:foreign-pointer))

(defstruct (pile-context (:constructor %make-pile-context))
  (root +null-element+ :type pile-element)
  (stack (make-array 20 :element-type 'pile-element
                     :initial-element +null-element+
                     :fill-pointer 0
                     :adjustable t)
         :type (array pile-element (*))))

(defun pile-root (context)
  (pile-context-root context))

(defun context-head-index (context)
  (declare (pile-context context))
  (1- (length (pile-context-stack context))))

(defun context-head (context)
  (declare (pile-context context))
  (aref (pile-context-stack context) (context-head-index context)))

(defun (setf context-head) (value context)
  (declare (pile-context context)
           (pile-element value))
  (setf (aref (pile-context-stack context) (context-head-index context))
        value))

(defun pile-push (element context)
  (declare (pile-context context)
           (pile-element element))
  (vector-push-extend element (pile-context-stack context))
  context)

(defun pile-pop (context)
  (declare (pile-context context))
  (let ((old-index (context-head-index context)))
    (prog1 (vector-pop (pile-context-stack context))
      (setf (aref (pile-context-stack context) old-index)
            +null-element+))))

(defun pile-peek (context &optional (n 1))
  (let* ((from (context-head-index context))
         (to (- from n)))
    (loop :for i :from from :downto to :collect
       (aref (pile-context-stack context) i))))
