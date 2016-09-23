(in-package #:luis-a-ui)

;;----------------------------------------------------------------------

(deftclass (nk-cepl-render-data (:conc-name nil))
  (cmds (foreign-alloc '(:struct nk-buffer)))
  (null-tex (foreign-alloc '(:struct nk-draw-null-texture)))
  (viewport (cepl:make-viewport))
  anti-aliasing
  vert-array
  elem-array
  vert-stream
  font-tex
  font-sampler)

;;----------------------------------------------------------------------

(deftclass (nk-cepl-root (:conc-name nil))
  win
  render-data
  (nk-ctx (foreign-alloc '(:struct nk-context)))
  (atlas (foreign-alloc '(:struct nk-font-atlas))))

;;----------------------------------------------------------------------

(defun render-data-upload-atlas (root image width height)
  (let* ((render-data (render-data root))
         (carr (cepl:make-c-array-from-pointer
                (list width height) :uint8-vec4 image))
         (tex (cepl:make-texture carr)))
    ;; (loop for y below 20 do
    ;;      (loop for x below 20 do
    ;;           (print (cepl:aref-c carr x y))))
    (setf (font-tex render-data) tex
          (font-sampler render-data) (cepl:sample tex))))

(defun nk-cepl-font-stash-begin (root)
  (let ((atlas (atlas root)))
    (nk-font-atlas-init-default atlas)
    (nk-font-atlas-begin atlas)
    root))

(defun nk-cepl-font-stash-end (root)
  (let ((atlas (atlas root)))
    (with-foreign-objects ((w :int) (h :int))
      (let ((image (nk-font-atlas-bake atlas w h nk-font-atlas-rgba32)))
        (render-data-upload-atlas root image (mem-aref w :int) (mem-aref h :int))
        (with-slots (null-tex font-tex) (render-data root)
          (nk-font-atlas-end atlas (nk-handle-id (cepl:texture-id font-tex))
                             null-tex))
        (with-foreign-slots ((default-font) atlas (:struct nk-font-atlas))
          (unless (null-pointer-p default-font)
            (nk-style-set-font
             (nk-ctx root)
             (foreign-slot-pointer default-font '(:struct nk-font) 'handle))))))
    root))

(defvar *nk-cepl-root* nil)

;;----------------------------------------------------------------------
;; setup

(defvar *initd* nil)

(defun init-render-data ()
  (let* ((vert-array (cepl:make-gpu-array
                      nil :element-type 'nuklear-graphics:nk-cepl-vertex
                      :dimensions 32768
                      :access-style :stream-draw))
         (elem-array (cepl:make-gpu-array
                      nil :element-type :ushort
                      :dimensions 262144
                      :access-style :stream-draw))
         (vert-stream (cepl:make-buffer-stream
                       vert-array :index-array elem-array
                       :retain-arrays t))
         (result
          (make-nk-cepl-render-data :vert-array vert-array
                                    :elem-array elem-array
                                    :vert-stream vert-stream)))
    (nk-buffer-init-default (cmds result))
    (multiple-value-bind (w h)
        (sdl2:get-window-size  cepl.context::*GL-window*)
      (format t "yo: ~s ~s" w h)
      (setf (cepl:viewport-resolution (viewport result))
            (cepl:v! w h)))
    result))

(defun init-nk-root ()
  (let ((root (make-nk-cepl-root)))
    (nk-init-default (nk-ctx root) (null-pointer))
    (setf (render-data root) (init-render-data))
    (cepl:with-viewport (viewport (render-data root))
      (nk-cepl-font-stash-begin root)
      (nk-cepl-font-stash-end root))
    root))

(defun init-all ()
  (unless *initd*
    (unless cepl.context:*gl-context*
      (cepl:repl))
    (cepl:step-host)
    (setf *nk-cepl-root* (init-nk-root))
    (skitter:listen-to (lambda (x y) (mouse-listener x y))
                       (skitter:mouse 0) :pos)
    (skitter:listen-to (lambda (x y) (system-listener x y))
                       skitter:+system+ :quitting)
    (skitter:listen-to (lambda (x y) (window-size-callback x y))
                       (skitter:window 0) :size)
    (setf *initd* t)))

;;----------------------------------------------------------------------

(defun render-ui (root)
  (with-slots (nk-ctx render-data) root
    (gl:blend-equation :func-add)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :scissor-test :blend)
    (gl:disable :cull-face :depth-test)
    ;;
    (with-foreign-object (vert-layout '(:struct nk-draw-vertex-layout-element) 4)
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 0)
            `(attribute ,nk-vertex-position
                        test-c2ffi::format ,nk-format-float
                        offset 0))
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 1)
            `(attribute ,nk-vertex-texcoord
                        test-c2ffi::format ,nk-format-float
                        offset 8))
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 2)
            `(attribute ,nk-vertex-color
                        test-c2ffi::format ,nk-format-r8g8b8a8
                        offset 16))
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 3)
            `(attribute ,nk-vertex-attribute-count
                        test-c2ffi::format ,nk-format-count
                        offset 0))
      ;;
      (with-foreign-object (config '(:struct nk-convert-config))
        (zero-out config '(:struct nk-convert-config))
        (with-foreign-slots
            ((vertex-layout
              vertex-size vertex-alignment test-c2ffi::null
              circle-segment-count curve-segment-count arc-segment-count
              global-alpha shape-aa line-aa) config (:struct nk-convert-config))
          (setf vertex-layout vert-layout
                vertex-size 20
                vertex-alignment 1
                test-c2ffi::null (null-tex render-data)
                circle-segment-count 22
                curve-segment-count 22
                arc-segment-count 22
                global-alpha 1s0
                shape-aa 0
                line-aa 0))
        (cepl:with-gpu-array-as-pointer (vert-ptr (vert-array render-data)
                                                  :access-type :write-only
                                                  :target :array-buffer)
          (cepl:with-gpu-array-as-pointer (elem-ptr (elem-array render-data)
                                                    :access-type :write-only
                                                    :target :element-array-buffer)
            (with-foreign-objects ((v-buf '(:struct nk-buffer))
                                   (e-buf '(:struct nk-buffer)))
              (nk-buffer-init-fixed
               v-buf vert-ptr (garr-size (vert-array render-data)))
              (nk-buffer-init-fixed
               e-buf elem-ptr (garr-size (elem-array render-data)))
              (nk-convert nk-ctx (cmds render-data) v-buf e-buf config))))))

    (let* ((vsize (cepl:viewport-resolution (cepl:current-viewport)))
           (vw (v:x vsize))
           (vh (v:y vsize))
           (ortho (cepl:m! (/ 2 vw)  0          0 -1
                           0         (/ -2 vh)  0  1
                           0         0         -1  0
                           0         0          0  1))
           (scale-x 1s0)
           (scale-y 1s0)
           (offset 0))
      (with-slots (vert-stream) render-data
        (with-foreign-objects ((cmd '(:struct nk-draw-command)))
          (setf cmd (nk--draw-begin nk-ctx (cmds render-data)))
          (loop :while (not (null-pointer-p cmd)) :do
             (with-foreign-slots ((elem-count) cmd (:struct nk-draw-command))
               (let ((clip-rect (foreign-slot-pointer cmd '(:struct nk-draw-command) 'clip-rect)))
                 (when (> elem-count 0)
                   (with-foreign-slots ((x y w h) clip-rect (:struct nk-rect))
                     (gl:scissor (* x scale-x) (* (- vh (+ y h)) scale-y)
                                 (* w scale-x) (* h scale-y)))
                   (setf (cepl:buffer-stream-length vert-stream) elem-count)
                   (setf (cepl.types::buffer-stream-start vert-stream) offset)
                   (cepl:map-g #'nuklear-graphics:nk-basic
                               vert-stream
                               :tex (font-sampler render-data)
                               :proj-mtx ortho)
                   (incf offset elem-count))))
             (setf cmd (nk--draw-next cmd (cmds render-data) nk-ctx))))))
    (nk-clear nk-ctx)
    ;;
    (gl:disable :scissor-test :blend)
    (gl:enable :depth-test :cull-face)))

(defun carr-size (c-arr)
  (assert (= 1 (length (cepl:c-array-dimensions c-arr))))
  (cepl.types::c-array-row-byte-size c-arr))

(defun garr-size (g-arr)
  (cepl.types::gpu-array-bb-byte-size g-arr))

(defun zero-out (ptr type &optional (count 1))
  (%memset ptr 0 (* count (foreign-type-size type))))

(defcfun (%memset "memset") :pointer
  (destination-pointer :pointer)
  (val :int)
  (byte-length :long))

;;----------------------------------------------------------------------

(defparameter *rect* '(h 250s0 w 210s0 y 200s0 x 200s0))

(defparameter *flags* (logior nk-window-border
                              nk-window-movable
                              nk-window-closable
                              nk-window-scalable
                              nk-window-minimizable
                              nk-window-title))

(defun step-example ()
  (with-slots (nk-ctx render-data) *nk-cepl-root*
    (cepl:with-viewport (viewport render-data)
      ;; input
      (nk-input-begin nk-ctx)
      (nk-input-end nk-ctx)
      ;; gui
      (with-foreign-objects ((layout '(:struct nk-panel))
                             (val :int))
        (setf (mem-aref val :int) 20)
        (with-foreign-strings ((title "Nuklear UI")
                               (blbl "In Lisp!")
                               (juice "JUICE!:"))
          (when (= 1 (nk-begin nk-ctx layout title *rect* *flags*))
            (nk-layout-row-static nk-ctx 30s0 80 1)
            (when (= 1 (nk-button-label nk-ctx blbl))
              (print "Holy shite!"))
            (nk-layout-row-dynamic nk-ctx 20s0 1)
            (nk-property-int nk-ctx juice 0 val 100 10 1s0)
            (nk-layout-row-dynamic nk-ctx 120s0 1)
            (nk-color-picker nk-ctx '(r 28 g 48 b 62 a 0) nk-rgba)
            (nk-end nk-ctx))))
      ;; draw
      ;;(cepl:clear)
      (gl:clear :color-buffer-bit)
      (gl:clear-color 0.109 0.188 0.243 0s0)
      (render-ui *nk-cepl-root*)))
  (cepl:swap))

(defun reshape (new-dimensions)
  (let ((new-dimensions (cepl:v! (v:x new-dimensions) (v:y new-dimensions))))
    (print new-dimensions)
    (setf (cepl:viewport-resolution (viewport (render-data *nk-cepl-root*)))
          new-dimensions)))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (reshape (skitter:size-2d-vec event)))

(defun system-listener (event timestamp)
  (declare (ignore event timestamp))
  (stop-loop))

(defun mouse-listener (event timestamp)
  (let* ((d (skitter:xy-pos-vec event)))
    d))

;;----------------------------------------------------------------------

(defvar *running* nil)

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
