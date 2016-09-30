(in-package :pile)

(defstruct pile-element)

(defstruct (pile-semantic-element (:include pile-element))
  (name (error "semantic element must have a name") :type keyword))

(defstruct (pile-site-element (:include pile-semantic-element)))

(defstruct (pile-nk-ptr-element (:include pile-element))
  (ptr (error "pointer to nk element must be provided")
       :type cffi:foreign-pointer))
