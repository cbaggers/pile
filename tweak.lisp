(in-package :pile)
(in-readtable :fn.reader)

(defvar *tweak-root* nil)
(defvar *tweak-ctx* nil)
(defvar *tweaking* nil)
(defvar *tweak-count*)

(defun ensure-initialized ()
  (unless *tweak-root*
    (when cepl.context:*gl-context*
      (setf *tweak-root* (pile:make-root-element)))))

(defun tweak-begin ()
  (nk-input-begin (pile-nk-ptr-element-ptr *tweak-root*))
  (unwind-protect (replay-events *tweak-root*)
    (nk-input-end (pile-nk-ptr-element-ptr *tweak-root*))))

(defun get-tweak-context ()
  (ensure-initialized)
  (make-context-from-root-element *tweak-root*))

(defmacro with-tweak-no-render (&body body)
  `(let ((*tweak-ctx* (get-tweak-context))
         (*tweak-count* 0))
     (tweak-begin)
     ,@body))

(defun tweak-finalize ()
  (nk-clear (pile-nk-ptr-element-ptr *tweak-root*)))

(defmacro with-tweak-no-finalize (&body body)
  `(unwind-protect (with-tweak-no-render ,@body)
     (tweak-render)))

(defun tweak-render ()
  (%render-ui-from-root *tweak-root*))

(defmacro with-tweak (&body body)
  `(prog1 (with-tweak-no-finalize
            ,@body)
     (tweak-finalize)))

(defmacro tweak (place)
  `(tweak-dispatch ,place (lambda (val) (setf ,place val))))

(defun tweak-dispatch (value setter)
  (assert *tweak-ctx* () "Not within a tweak scope.")
  (if *tweaking*
      (tweak-inner value setter)
      (let ((*tweaking* t))
        (tweak-outer value setter))))

(defun tweak-outer (value setter)
  (let ((panel-name (format nil "tweak-~a" (incf *tweak-count*))))
    (with-ui-context *tweak-ctx*
      (in-panel (:title panel-name :bounds (rtg-math::v! 100s0 40s0 210s0 220s0)
                        :flags '(:border :movable :scalable :title))
        (funcall setter (tweak-object value))))))

(defmethod tweak-object (object)
  (with-ui-context *tweak-ctx*
    (in-row-static (:height 20s0 :item-width 180)
      (text :val (format nil "~s" object))))
  object)

(defmethod tweak-object ((object integer))
  (with-ui-context *tweak-ctx*
    (in-row (:height 20s0)
      (property-int :text "val" :val object :min -1000 :max 1000 :step 1))))

(defmethod tweak-object ((object single-float))
  (with-ui-context *tweak-ctx*
    (in-row (:height 20s0)
      (property-float :text "val" :val object :min -1000f0 :max 1000f0 :step 1))))

(defmethod tweak-object ((object array))
  (typecase object
    (rtg-math.types:vec3 (tweak-vec3 object))
    (t (tweak-unknown object))))

(defmethod tweak-unknown (object)
  (with-ui-context *tweak-ctx*
    (in-row (:height 20s0)
      (label :text "A picker, so it is"))))

(defmethod tweak-vec3 (object)
  (with-ui-context *tweak-ctx*
    (in-row (:height 20s0)
      (rtg-math:v!
       (property-float :text "x" :val (v:x object) :min -100f0 :max 100f0 :step 0.1)
       (property-float :text "y" :val (v:y object) :min -100f0 :max 100f0 :step 0.1)
       (property-float :text "z" :val (v:z object) :min -100f0 :max 100f0 :step 0.1)))))

(defun tweak-inner (value setter)
  (error "Implement me! ~a ~a" value setter)
  value)
