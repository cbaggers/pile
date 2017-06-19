(in-package :pile)

;;------------------------------------------------------------

(defmacro step-host-&-dispatch-events ()
  `(cepl:step-host (pile-root ,+ctx+)))

(defmacro dispatch-events ()
  `(replay-events (pile-root ,+ctx+)))

;;------------------------------------------------------------

(defun cache-event (root-element func args)
  (let ((events (root-element-cached-events root-element)))
    (vector-push-extend (cons func args) events)))

(defun replay-events (root-element)
  (let ((events (root-element-cached-events root-element)))
    (loop :for (func . args) :across events :do
       (apply func (cons (pile-nk-ptr-element-ptr root-element) args)))
    (setf (fill-pointer events) 0)))

;;------------------------------------------------------------

(defun window-size-callback (root-element new-dimensions)
  ;;(print new-dimensions)
  (pile:reshape root-element (v! new-dimensions)))

;;------------------------------------------------------------

(defun mouse-pos-listener (root-element pos)
  (declare (ignore timestamp))
  (let ((nk-ptr (pile::pile-nk-ptr-element-ptr root-element)))
    ;;
    (cache-event root-element #'nk-input-motion
                 (multiple-value-list
                  (unpack-skitter-mouse-pos nk-ptr pos)))))

(defun unpack-skitter-mouse-pos (nk-ptr pos)
  (let* ((mouse-ptr (c-ptr nk-ptr (:struct nk-context) input mouse))
         (grabbed (c-val mouse-ptr (:struct nk-mouse) grabbed))
         (mouse (mouse 0)))
    ;;
    (if (= grabbed 1)
        (let* ((prev-ptr (c-ptr mouse-ptr (:struct nk-mouse) prev))
               (prev-x (c-val prev-ptr (:struct nk-vec2) x))
               (prev-y (c-val prev-ptr (:struct nk-vec2) y))
               (delta (mouse-move mouse))
               (new-x (floor (+ prev-x (v:x delta))))
               (new-y (floor (+ prev-y (v:y delta)))))
          (values new-x new-y))
        (let* ((x (floor (v:x pos)))
               (y (floor (v:y pos))))
          (values x y)))))

;;------------------------------------------------------------

(defun keyboard-listener (root-element pressed key-id)
  (let* ((nk-ptr (pile::pile-nk-ptr-element-ptr root-element)))
    (flet ((cache-input-key (key)
               (cache-event root-element #'nk-input-key
                            (list key (convert-to-foreign pressed :boolean)))))
      (case (sdl2:scancode-symbol key-id)
        (:scancode-return (cache-input-key NK-KEY-ENTER))
        (:scancode-backspace (cache-input-key NK-KEY-BACKSPACE))
        (:scancode-left (cache-input-key NK-KEY-LEFT))
        (:scancode-right (cache-input-key NK-KEY-RIGHT))
        (t (when pressed
             (cache-event root-element #'nk-input-unicode
                          (list (sdl2:get-key-from-scancode key-id)))))))))

;;------------------------------------------------------------

(defun mouse-button-listener (root-element data index)
  (declare (ignore timestamp))
  (let ((nk-ptr (pile::pile-nk-ptr-element-ptr root-element)))
    (cache-event root-element #'nk-input-button
                 (multiple-value-list
                  (unpack-skitter-mouse-button data index)))))

(defun unpack-skitter-mouse-button (down-p b-id)
  (let* ((pos (mouse-pos (mouse 0)))
         (nk-id (cepl-utils:case= b-id
                  (mouse.left nk-button-left)
                  (mouse.middle nk-button-middle)
                  (mouse.right nk-button-right))))
    (values nk-id (floor (v:x pos)) (floor (v:y pos)) (if down-p 1 0))))

;;------------------------------------------------------------
