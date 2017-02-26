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
(defparameter *another-val* 10)
(defparameter *some-color* (v! 28 45 62 1))

(defui test-win (title x y)
  (in-panel (:title title :bounds (v! x y 210s0 150s0))
    (in-row-static (:height 30s0 :item-width 180)
      (when (button-label :text "In Lisp!")
        (print "Hot damn!")))
    (in-row (:height 40s0) (button-label :text "This"))
    (in-row (:height 100s0)
      (column-graph :data (loop for i below 10 :by 0.4 collect (sin i))))))

(defun test-fn (ctx)
  (with-ui-context ctx
    (in-row (:height 20s0)
      (option-label :text "HOOOO" :active t))))

(defun step-example ()
  (clear)
  (step-host)
  ;;
  (in-ui *ui-root*
    (in-panel (:title "Nuklear UI!" :bounds (v! 200s0 40s0 210s0 250s0))
      (in-row-static (:height 30s0 :item-width 180)
        (when (button-label :text "In Lisp!")
          (print "Hot damn!")))
      (in-row (:height 20s0)
        (setf *some-val* (property-float :text "JUICE!" :val *some-val*
                                         :min 0s0 :max 100s0 :step 0.1s0)))
      (in-row (:height 20s0)
        (property-int :text "int prop" :min -10 :max 10 :step 10))
      (in-row (:height 20s0)
        (progress :current *some-val* :max 100))
      (ui-call #'test-fn)
      (in-row (:height 20s0)
        (check-label :text "TWOOOO" :active nil))
      (in-row (:height 20s0)
        (text :val "Suup! Apparently"))
      (in-row (:height 100s0)
        (line-graph :data (loop for i below 10 :by 0.4 collect (sin i))))
      (in-row (:height 30s0)
        (setf *another-val*
              (slide-int :val (floor *another-val*) :min 0 :max 200
                         :step 10)))
      (in-row (:height 20s0)
        (label :text "A picker, so it is"))
      (in-row (:height 220s0)
        (setf *some-color* (color-picker :color *some-color* :format :rgba))))
    ;;
    (test-win :title "MoreWindows" :x 50s0 :y 300s0)
    (test-win :title "yes" :x 250s0 :y 400s0)
    ;;
    (render-ui))
  (cepl:swap)
  (gl:clear-color (/ (v:x *some-color*) 255)
                  (/ (v:y *some-color*) 255)
                  (/ (v:z *some-color*) 255)
                  0))

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
  (declare (ignore event timestamp tpref))
  (stop-loop))
