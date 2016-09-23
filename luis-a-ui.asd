;;;; luis-a-ui.asd

(asdf:defsystem #:luis-a-ui
  :description "Tried to be a nicer api over Nuklear for CEPL"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (:cepl.sdl2
               :dirt :nineveh :structy-defclass :cepl.skitter.sdl2
               :fn :named-readtables :livesupport :test-c2ffi.ffi
               :dendrite.primitives)
  :serial t
  :components ((:file "package")
               (:file "base-graphics")
               (:file "base")))
