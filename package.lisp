;;;; package.lisp

(defpackage #:pile.renderer
  (:use #:cl #:cepl :cffi)
  (:export :nk-basic :nk-cepl-vertex))

(defpackage #:pile
  (:use #:cl #:structy-defclass :cffi #:raw-bindings-nuklear
        :cffi :alexandria)
  (:export :init-all :reshape :step-ui))
