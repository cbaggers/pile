(in-package #:pile.renderer)

;;----------------------------------------------------------------------

(defstruct-g nk-cepl-vertex
  (position :vec2 :accessor pos)
  (texture :vec2 :accessor uv)
  (color :uint8-vec4 :accessor col))

(defun-g nk-vert ((vert nk-cepl-vertex) &uniform (proj-mtx :mat4))
  (values (* proj-mtx (v! (pos vert) 0 1))
          (uv vert)
          (/ (col vert) (v! 255s0 255s0 255s0 1s0)))) ;; this divide is because I didnt normalize in the struct

(defun-g nk-frag ((tc :vec2) (col :vec4) &uniform (tex :sampler-2d))
  (let* ((t-col (texture tex tc)))
    (* col t-col)
    ;; (* (v! (s~ col :xyz) (/ 1 (v:w t-col)))
    ;;    t-col)
    ))

(defpipeline-g nk-basic ()
  (nk-vert nk-cepl-vertex)
  (nk-frag :vec2 :vec4))

;;----------------------------------------------------------------------
