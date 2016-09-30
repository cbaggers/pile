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
    (skitter:listen-to (lambda (x y) (mouse-listener x y))
                       (skitter:mouse 0) :pos)
    (skitter:listen-to (lambda (x y) (system-listener x y))
                       skitter:+system+ :quitting)
    (skitter:listen-to (lambda (x y) (window-size-callback x y))
                       (skitter:window 0) :size)
    (setf *initd* t)))

(defparameter *some-val* 10)

(defun step-example ()
  (in-ui *ui-root*
    (in-input)

    (in-panel (:title "Nuklear UI!" :bounds (v! 200s0 40s0 210s0 250s0))
      (in-row-static (:height 30s0 :item-width 180)
        (when (button-label :text "In Lisp!")
          (print "Hot damn!")))
      (in-row (:height 20s0)
        (property-int :text "JUICE!" :val *some-val* :min 0 :max 100 :step 10))
      (in-row (:height 20s0)
        (property-int :text "Name" :min 0 :max 100 :step 10))
      (in-row (:height 20s0)
        (property-int :text "Thing" :min 0 :max 100 :step 10))
      (in-row (:height 120s0)
        (color-picker :color (v! 28 45 62 0) :format :rgba)))

    (test-win :title "more" :x 50s0 :y 300s0)

    (test-win :title "yes" :x 250s0 :y 400s0)

    (gl:clear :color-buffer-bit)
    (render-ui)
    (cepl:swap)))

(defui test-win (title x y)
  (in-panel (:title title :bounds (v! x y 210s0 150s0))
    (in-row-static (:height 30s0 :item-width 180)
      (when (button-label :text "In Lisp!")
        (print "Hot damn!")))
    (in-row (:height 40s0) (button-label :text "yup"))))

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
     (livesupport::continuable (cepl:step-host))
     (livesupport::continuable (step-example))
     :until (or (not *running*) (= (floor count) 0)))
  (print "stopped")
  (setf *running* nil))


(defun stop-loop ()
  (setf *running* nil))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (let ((new-dimensions (skitter:size-2d-vec event)))
    (print new-dimensions)
    (pile:reshape *ui-root* new-dimensions)))

(defun system-listener (event timestamp)
  (declare (ignore event timestamp))
  (stop-loop))

(defun mouse-listener (event timestamp)
  (declare (ignore timestamp))
  (let* ((d (skitter:xy-pos-vec event)))
    d))
