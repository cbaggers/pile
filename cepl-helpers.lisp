(in-package :pile)

(defun carr-size (c-arr)
  (assert (= 1 (length (cepl:c-array-dimensions c-arr))))
  (cepl.types::c-array-row-byte-size c-arr))

(defun garr-size (g-arr)
  (cepl.types::gpu-array-bb-byte-size g-arr))
