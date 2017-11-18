(in-package #:pile)

;;----------------------------------------------------------------------
;;

(defvar *vert-layout*
  (let ((vl (foreign-alloc '(:struct nk-draw-vertex-layout-element) :count 4)))
    (setf (mem-aref vl '(:struct nk-draw-vertex-layout-element) 0)
          `(attribute ,nk-vertex-position
                      raw-bindings-nuklear::format ,nk-format-float
                      offset 0))
    (setf (mem-aref vl '(:struct nk-draw-vertex-layout-element) 1)
          `(attribute ,nk-vertex-texcoord
                      raw-bindings-nuklear::format ,nk-format-float
                      offset 8))
    (setf (mem-aref vl '(:struct nk-draw-vertex-layout-element) 2)
          `(attribute ,nk-vertex-color
                      raw-bindings-nuklear::format ,nk-format-r8g8b8a8
                      offset 16))
    (setf (mem-aref vl '(:struct nk-draw-vertex-layout-element) 3)
          `(attribute ,nk-vertex-attribute-count
                      raw-bindings-nuklear::format ,nk-format-count
                      offset 0))
    vl))

(defun %render-ui (context)
  (%render-ui-from-root (pile-root context)))

(let ((ortho (cepl:m! 0  0  0 -1
                      0  0  0  1
                      0  0 -1  0
                      0  0  0  1)))
  (defun %render-ui-from-root (root-elem)
    (declare (optimize speed))
    (let* ((nk-ctx (root-element-ptr root-elem))
           (render-data (root-element-render-data root-elem)))
      (cepl:with-viewport (viewport render-data)
        (gl:blend-equation :func-add)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:enable :scissor-test :blend)
        (cepl-utils:with-setf*
            ((cepl:depth-test-function (cepl.context:cepl-context)) nil
             (cepl:cull-face (cepl.context:cepl-context)) nil)
          ;;
          (let ((vert-layout *vert-layout*))
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
                    (nk-convert nk-ctx (cmds render-data) v-buf e-buf config)))))
            (let* ((vsize (cepl:viewport-resolution (cepl:current-viewport)))
                   (vw (v:x vsize))
                   (vh (v:y vsize))
                   (scale-x 1)
                   (scale-y 1)
                   (offset 0))
              (declare (type (single-float 0s0 #.(float #xffff 0s0)) vw vh))

              (setf (m4:melm ortho 0 0) (the single-float (/ 2s0 vw)))
              (setf (m4:melm ortho 1 1) (/ -2s0 vh))

              (with-slots (vert-stream) render-data
                (with-foreign-objects ((cmd '(:struct nk-draw-command)))
                  (setf cmd (nk--draw-begin nk-ctx (cmds render-data)))
                  (loop :while (not (null-pointer-p cmd)) :do
                     (with-foreign-slots ((elem-count (:pointer clip-rect)) cmd
                                          (:struct nk-draw-command))
                       (when (> elem-count 0)
                         (with-foreign-slots ((raw-bindings-nuklear::x
                                               raw-bindings-nuklear::y
                                               raw-bindings-nuklear::w
                                               raw-bindings-nuklear::h)
                                              clip-rect (:struct nk-rect))
                           (let ((x raw-bindings-nuklear::x)
                                 (y raw-bindings-nuklear::y)
                                 (w raw-bindings-nuklear::w)
                                 (h raw-bindings-nuklear::h))
                             (declare (type (single-float 0s0 #.(float #xffff 0s0))
                                            x y w h))
                             (gl:scissor (* x scale-x) (* (- vh (+ y h)) scale-y)
                                         (* w scale-x) (* h scale-y))
                             (setf (cepl:buffer-stream-length vert-stream) elem-count)
                             (setf (cepl.types::buffer-stream-start vert-stream) offset)
                             (cepl:map-g #'pile.renderer:nk-basic
                                         vert-stream
                                         :tex (font-sampler render-data)
                                         :proj-mtx ortho))
                           (incf offset elem-count))))
                     (setf cmd (nk--draw-next cmd (cmds render-data) nk-ctx))))))
            ;;
            (gl:disable :scissor-test :blend)))))))

;;----------------------------------------------------------------------
