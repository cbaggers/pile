(in-package :pile)


(defstruct (root-element (:include pile-nk-ptr-element)
                         (:constructor %make-root-element))
  win
  render-data
  (atlas (foreign-alloc '(:struct nk-font-atlas))))


(defun make-root-element ()
  (let ((nk-ctx (foreign-alloc '(:struct nk-context))))
    (nk-init-default nk-ctx (null-pointer))
    (let* ((render-data (init-render-data))
           (atlas (init-fonts nk-ctx render-data)))
      (%make-root-element
       :ptr nk-ctx
       :render-data render-data
       :atlas atlas))))


(defun make-context-from-root-element (root-elem)
  (assert cepl.context:*gl-context*)
  (let ((ctx (make-pile-context)))
    (pile-push root-elem ctx)))


(defun reshape (root-element new-dimensions)
  (let ((new-dimensions (cepl:v! (v:x new-dimensions) (v:y new-dimensions)))
        (render-data (root-element-render-data root-element)))
    (setf (cepl:viewport-resolution (viewport render-data))
          new-dimensions)))


;;------------------------------------------------------------
;; Find homes for all this stuff

(defun init-fonts (nk-ctx render-data)
  (cepl:with-viewport (viewport render-data)
    (let ((atlas (font-stash-begin
                  (foreign-alloc '(:struct nk-font-atlas)))))
      (font-stash-end nk-ctx atlas render-data))))

(defun render-data-upload-atlas (render-data image width height)
  (let* ((carr (cepl:make-c-array-from-pointer
                (list width height) :uint8-vec4 image))
         (tex (cepl:make-texture carr)))
    (setf (font-tex render-data) tex
          (font-sampler render-data) (cepl:sample tex))))

(defun font-stash-begin (atlas)
  (nk-font-atlas-init-default atlas)
  (nk-font-atlas-begin atlas))

(defun font-stash-end (nk-ctx atlas render-data)
  (with-foreign-objects ((w :int) (h :int))
    (let ((image (nk-font-atlas-bake atlas w h nk-font-atlas-rgba32)))
      (render-data-upload-atlas render-data image (mem-aref w :int) (mem-aref h :int))
      (with-slots (null-tex font-tex) render-data
        (nk-font-atlas-end atlas (nk-handle-id (cepl:texture-id font-tex))
                           null-tex))
      (with-foreign-slots ((default-font) atlas (:struct nk-font-atlas))
        (unless (null-pointer-p default-font)
          (nk-style-set-font
           nk-ctx
           (foreign-slot-pointer default-font '(:struct nk-font) 'handle))))))
  atlas)

;;----

(deftclass (render-data (:conc-name nil))
  (cmds (foreign-alloc '(:struct nk-buffer)))
  (null-tex (foreign-alloc '(:struct nk-draw-null-texture)))
  (viewport (cepl:make-viewport))
  anti-aliasing
  vert-array
  elem-array
  vert-stream
  font-tex
  font-sampler)

(defun init-render-data ()
  (let* ((vert-array (cepl:make-gpu-array
                      nil :element-type 'pile.renderer:nk-cepl-vertex
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
          (make-render-data :vert-array vert-array
                                    :elem-array elem-array
                                    :vert-stream vert-stream)))
    (nk-buffer-init-default (cmds result))
    (multiple-value-bind (w h)
        (sdl2:get-window-size cepl.context::*GL-window*)
      (setf (cepl:viewport-resolution (viewport result))
            (cepl:v! w h)))
    result))
