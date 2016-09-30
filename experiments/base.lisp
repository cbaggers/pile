(in-package #:pile.experiments)

(defvar *running* nil)
(defvar *initd* nil)
(defvar *ui-root* nil)

(defun init-all ()
  (unless *initd*
    (unless cepl.context:*gl-context*
      (cepl:repl))
    (setf *ui-root* (pile:make-root-element))
    (gl:clear-color 0.109 0.188 0.243 0s0)
    (skitter:listen-to (lambda (x y z) (mouse-pos-listener x y z))
                       (mouse 0) :pos)
    (skitter:listen-to (lambda (x y z) (mouse-button-listener x y z))
                       (mouse 0) :button)
    (skitter:listen-to (lambda (x y z) (keyboard-listener x y z))
                       (keyboard 0) :button)
    (skitter:listen-to (lambda (x y z) (system-listener x y z))
                       skitter:+system+ :quitting)
    (skitter:listen-to (lambda (x y z) (window-size-callback x y z))
                       (window 0) :size)
    (setf *initd* t)))

(defparameter *some-val* 10)

(defui test-win (title x y)
  (in-panel (:title title :bounds (v! x y 210s0 150s0))
    (in-row-static (:height 30s0 :item-width 180)
      (when (button-label :text "In Lisp!")
        (print "Hot damn!")))
    (in-row (:height 40s0) (button-label :text "yup"))
    (in-row (:height 40s0) (button-label :text "mooooor"))))

(defun step-example ()
  (clear)
  (in-ui *ui-root*
    (in-input
      (cepl:step-host *ui-root*))
    (in-panel (:title "Nuklear UI!" :bounds (v! 200s0 40s0 210s0 250s0))
      (in-row-static (:height 30s0 :item-width 180)
        (when (button-label :text "In Lisp!")
          (print "Hot damn!")))
      (in-row (:height 20s0)
        (setf *some-val* (property-int :text "JUICE!" :val *some-val*
                                       :min 0 :max 100 :step 10)))
      (in-row (:height 20s0)
        (property-int :text "Name" :min 0 :max 100 :step 10))
      (in-row (:height 20s0)
        (property-int :text "Thing" :min 0 :max 100 :step 10))
      (in-row (:height 120s0)
        (color-picker :color (v! 28 45 62 0) :format :rgba)))

    (test-win :title "more" :x 50s0 :y 300s0)

    (test-win :title "yes" :x 250s0 :y 400s0)
    (render-ui)
    (cepl:swap)))

(defun run-loop (&optional (count 0))
  (assert (>= count 0))
  ;;
  (init-all)
  ;;
  (setf *running* t)
  (print "started")
  (loop :do
     (decf count)
     (livesupport:update-repl-link)
     (livesupport::continuable (step-example))
     :until (or (not *running*) (= (floor count) 0)))
  (print "stopped")
  (setf *running* nil))


(defun stop-loop ()
  (setf *running* nil))

(defun window-size-callback (event timestamp tpref)
  (declare (ignore timestamp))
  (let ((new-dimensions (skitter:size-2d-vec event)))
    (print new-dimensions)
    (pile:reshape *ui-root* new-dimensions)))

(defun system-listener (event timestamp tpref)
  (declare (ignore event timestamp))
  (stop-loop))

(defun mouse-pos-listener (event timestamp tpref)
  (declare (ignore timestamp))
  (let* ((nk-ptr (pile::pile-nk-ptr-element-ptr tpref))
         (mouse-ptr
          (cffi:foreign-slot-pointer
           (cffi:foreign-slot-pointer nk-ptr '(:struct raw-bindings-nuklear:nk-context)
                                      'raw-bindings-nuklear:input)
           '(:struct raw-bindings-nuklear:nk-input)
           'raw-bindings-nuklear:mouse))
         (grabbed
          (cffi:foreign-slot-value
           mouse-ptr
           '(:struct raw-bindings-nuklear:nk-mouse)
           'raw-bindings-nuklear:grabbed)))
    ;;
    (if (= grabbed 1)
        (let* ((prev-ptr (cffi:foreign-slot-pointer
                          mouse-ptr
                          '(:struct raw-bindings-nuklear:nk-mouse)
                          'raw-bindings-nuklear:prev))
               (prev-x (cffi:foreign-slot-value
                        prev-ptr
                        '(:struct raw-bindings-nuklear:nk-vec2)
                        'raw-bindings-nuklear:x))
               (prev-y (cffi:foreign-slot-value
                        prev-ptr
                        '(:struct raw-bindings-nuklear:nk-vec2)
                        'raw-bindings-nuklear:y))
               (delta (skitter:xy-pos-relative event)))
          (raw-bindings-nuklear:nk-input-motion
           nk-ptr
           (floor (+ prev-x (v:x delta)))
           (floor (+ prev-y (v:y delta)))))
        (let ((pos (skitter:xy-pos-vec event)))
          (raw-bindings-nuklear:nk-input-motion
           nk-ptr (floor (v:x pos)) (floor (v:y pos)))))))

(defun keyboard-listener (event timestamp tpref)
  (declare (ignore event timestamp tpref))
  ;;(livesupport:peek event)
  )

(defun mouse-button-listener (event timestamp tpref)
  (declare (ignore timestamp))
  (let* ((nk-ptr (pile::pile-nk-ptr-element-ptr tpref))
         (pos (skitter:xy-pos-vec (skitter:mouse-pos (skitter:mouse 0))))
         (down-p (button-down-p event))
         (b-id (mouse.button-id event))
         (nk-id (cepl-utils:case= b-id
                  (mouse.left raw-bindings-nuklear:nk-button-left)
                  (mouse.middle raw-bindings-nuklear:nk-button-middle)
                  (mouse.right raw-bindings-nuklear:nk-button-right))))
    ;;
    (when nk-id
      (raw-bindings-nuklear:nk-input-button
       nk-ptr nk-id (floor (v:x pos)) (floor (v:y pos)) (if down-p 1 0)))))
