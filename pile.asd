;;;; pile.asd

(asdf:defsystem #:pile
  :description "Tried to be a nicer api over Nuklear for CEPL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (:cepl.sdl2
               :dirt :nineveh :structy-defclass :cepl.skitter.sdl2
               :fn :named-readtables :livesupport :raw-bindings-nuklear
               :dendrite.primitives)
  :serial t
  :components ((:file "package")
               (:file "cffi-helpers")
               (:file "cepl-helpers")
               (:file "renderer")
               (:file "element-types")
               (:file "context")
               (:file "root-element")
               (:file "input")
               (:file "basic-elements")
               (:file "render-ui")
               (:file "ui-compiling-macros")))
