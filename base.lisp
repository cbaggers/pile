(in-package #:pile)

;;----------------------------------------------------------------------

;; (deftclass (nk-cepl-root (:conc-name nil))
;;   win
;;   render-data
;;   (nk-ctx (foreign-alloc '(:struct nk-context)))
;;   (atlas (foreign-alloc '(:struct nk-font-atlas))))

;;----------------------------------------------------------------------

;; (defvar *nk-cepl-root* nil)

;;----------------------------------------------------------------------
;; setup

;; (defvar *initd* nil)

(defun init-all ()
  (unless *initd*
    (assert cepl.context:*gl-context*)
    (cepl:step-host)
    ;; (setf *nk-cepl-root* (init-nk-root))
    ;; (setf *initd* t)
    ))

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
                        raw-bindings-nuklear::format ,nk-format-float
                        offset 0))
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 1)
            `(attribute ,nk-vertex-texcoord
                        raw-bindings-nuklear::format ,nk-format-float
                        offset 8))
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 2)
            `(attribute ,nk-vertex-color
                        raw-bindings-nuklear::format ,nk-format-r8g8b8a8
                        offset 16))
      ;;
      (setf (mem-aref vert-layout '(:struct nk-draw-vertex-layout-element) 3)
            `(attribute ,nk-vertex-attribute-count
                        raw-bindings-nuklear::format ,nk-format-count
                        offset 0))
      ;;
      (with-foreign-object (config '(:struct nk-convert-config))
        (zero-out config '(:struct nk-convert-config))
        (with-foreign-slots
            ((vertex-layout
              vertex-size vertex-alignment raw-bindings-nuklear::null
              circle-segment-count curve-segment-count arc-segment-count
              global-alpha shape-aa line-aa) config (:struct nk-convert-config))
          (setf vertex-layout vert-layout
                vertex-size 20
                vertex-alignment 1
                raw-bindings-nuklear::null (null-tex render-data)
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
                   (cepl:map-g #'pile.renderer:nk-basic
                               vert-stream
                               :tex (font-sampler render-data)
                               :proj-mtx ortho)
                   (incf offset elem-count))))
             (setf cmd (nk--draw-next cmd (cmds render-data) nk-ctx))))))
    (nk-clear nk-ctx)
    ;;
    (gl:disable :scissor-test :blend)
    (gl:enable :depth-test :cull-face)))

;;----------------------------------------------------------------------

(defparameter *rect* '(h 250s0 w 210s0 y 200s0 x 200s0))

(defparameter *flags* (logior nk-window-border
                              nk-window-movable
                              nk-window-closable
                              nk-window-scalable
                              nk-window-minimizable
                              nk-window-title))

(defun step-ui-new (r-elem)
  (with-context r-elem
    (in-input)
    (in-panel (:title "Nuklear UI" :bounds (v! 200s0 200s0 210s0 250s0))
      (in-row-static (:height 30s0 :item-width 80)
        (when (button-label :text "In Lisp!")
          (print "Hot damn!")))
      (in-row-dynamic (:height 20s0)
        (property-int "JUICE!" :min 0 :max 100 :step 10))
      (in-row-dynamic (:height 120s0)
        (color-picker :color (v! 28 45 62 0) :format :rgba)))
    (gl:clear :color-buffer-bit)
    (gl:clear-color 0.109 0.188 0.243 0s0)
    (render-ui *nk-cepl-root*))
  (cepl:swap))

(defun reshape (new-dimensions)
  (let ((new-dimensions (cepl:v! (v:x new-dimensions) (v:y new-dimensions))))
    (print new-dimensions)
    (setf (cepl:viewport-resolution (viewport (render-data *nk-cepl-root*)))
          new-dimensions)))

;;----------------------------------------------------------------------
