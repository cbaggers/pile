;;;; test-ui-stuff.asd

(asdf:defsystem #:test-ui-stuff
  :description "Describe test-ui-stuff here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (:luis-a-ui :structy-defclass)
  :serial t
  :components ((:file "experiments/package")
               (:file "experiments/base")))
