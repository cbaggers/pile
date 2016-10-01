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
    (skitter:listen-to (lambda (x y z) (system-listener x y z))
                       skitter:+system+ :quitting)
    (setf *initd* t)))

(defparameter *some-val* 10)
(defparameter *some-color* (v! 28 45 62 1))

(defui test-win (title x y)
  (in-panel (:title title :bounds (v! x y 210s0 150s0))
    (in-row-static (:height 30s0 :item-width 180)
      (when (button-label :text "In Lisp!")
        (print "Hot damn!")))
    (in-row (:height 40s0) (button-label :text "yup"))))

(defun step-example ()
  (clear)
  (in-ui *ui-root*
    (in-input
      (step-host-&-dispatch-events))
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
        (setf *some-color* (color-picker :color *some-color* :format :rgba))))

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

(defun system-listener (event timestamp tpref)
  (declare (ignore event timestamp))
  (stop-loop))
