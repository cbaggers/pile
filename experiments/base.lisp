(in-package #:test-ui-stuff)

(defvar *running* nil)
(defvar *initd* nil)

(defun init-all ()
  (unless *initd*
    (unless cepl.context:*gl-context*
      (cepl:repl))
    (luis-a-ui:init-all)
    (skitter:listen-to (lambda (x y) (mouse-listener x y))
                       (skitter:mouse 0) :pos)
    (skitter:listen-to (lambda (x y) (system-listener x y))
                       skitter:+system+ :quitting)
    (skitter:listen-to (lambda (x y) (window-size-callback x y))
                       (skitter:window 0) :size)
    (setf *initd* t)))

(defun step-example ()
  (luis-a-ui:step-ui))

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
  (luis-a-ui:reshape (skitter:size-2d-vec event)))

(defun system-listener (event timestamp)
  (declare (ignore event timestamp))
  (stop-loop))

(defun mouse-listener (event timestamp)
  (let* ((d (skitter:xy-pos-vec event)))
    d))
