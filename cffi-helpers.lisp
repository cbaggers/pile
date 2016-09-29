(in-package #:pile)

(defcfun (%memset "memset") :pointer
  (destination-pointer :pointer)
  (val :int)
  (byte-length :long))

(defun zero-out (ptr type &optional (count 1))
  (%memset ptr 0 (* count (foreign-type-size type))))
