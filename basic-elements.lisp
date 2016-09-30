(in-package :pile)

(defvar +ctx+ 'nk-ctx)

;;------------------------------------------------------------

(defun make-root-element ()
  (let ((nk-ctx (foreign-alloc '(:struct nk-context))))
    (nk-init-default nk-ctx (null-pointer))
    (let* ((render-data (init-render-data))
           (atlas (init-fonts nk-ctx render-data))
           (elem (%make-root-element
                  :ptr nk-ctx
                  :render-data render-data
                  :atlas atlas))
           (ws-listener (lambda (x y z) (mouse-pos-listener elem x y z)))
           (mp-listener (lambda (x y z) (mouse-button-listener elem x y z)))
           (mb-listener (lambda (x y z) (keyboard-listener elem x y z)))
           (kb-listener (lambda (x y z) (window-size-callback elem x y z))))
      (setf (root-element-window-size-listener elem) ws-listener
            (root-element-mouse-pos-listener elem) mp-listener
            (root-element-mouse-button-listener elem) mb-listener
            (root-element-keyboard-listener elem) kb-listener)
      (skitter:listen-to ws-listener (skitter:mouse 0) :pos)
      (skitter:listen-to mp-listener (skitter:mouse 0) :button)
      (skitter:listen-to mb-listener (skitter:keyboard 0) :button)
      (skitter:listen-to kb-listener (skitter:window 0) :size)
      elem)))

;;------------------------------------------------------------

;; Maybe we allow the user to pass an existing the root element OR a context
;; this way we can make it easy to pass control to regular functions

(defmacro in-ui (root-element &body body)
  `(let ((,+ctx+ (make-context-from-root-element ,root-element)))
     (unwind-protect (progn ,@body)
       (nk-clear (pile-nk-ptr ,+ctx+)))))

;;------------------------------------------------------------

;; what if not =1? silent fail is ugly
(defmacro in-input (&body body)
  `(progn
     (nk-input-begin (pile-nk-ptr ,+ctx+))
     (unwind-protect (progn ,@body)
       (nk-input-end (pile-nk-ptr ,+ctx+)))))

;;------------------------------------------------------------

;; {NOTE} this was in-layout but we are making an nk-panel and so is renamed
;;        for now. Can anything else be the layout?
;;
;; what if not =1? silent fail is ugly
(defmacro in-panel ((&key title (bounds (cepl:v! 50s0 50s0 200s0 250s0))
                          (flags '(:border :movable :closable :scalable
                                   :minimizable :title)))
                    &body body)
  (assert title)
  (let ((flags (if (every #'keywordp flags)
                   (window-flags flags)
                   flags)))
    (with-gensyms (elem layout)
      `(with-foreign-object (,layout '(:struct nk-panel))
         (let ((,elem (make-pile-nk-ptr-element :ptr ,layout)))
           (when (= 1 (%nk-begin ,+ctx+ ,layout ,title ,bounds ,flags))
             (unwind-protect
                  (progn
                    (pile-push ,elem ,+ctx+)
                    ,@body)
               (pile-pop ,+ctx+)
               (nk-end (pile-nk-ptr ,+ctx+)))))))))

;; This sucks, make proper readers etc
(defun vec4-as-rect (v4)
  `(h ,(v:w v4) w ,(v:z v4) y ,(v:y v4) x ,(v:x v4)))

(defun window-flags (flags)
  (labels ((val (x f)
             (let ((val
                    (ecase f
                      (:border nk-window-border)
                      (:movable nk-window-movable)
                      (:scalable nk-window-scalable)
                      (:closable nk-window-closable)
                      (:minimizable nk-window-minimizable)
                      (:no-scrollbar nk-window-no-scrollbar)
                      (:title nk-window-title)
                      (:scroll-auto-hide nk-window-scroll-auto-hide)
                      (:background nk-window-background))))
               (logior x val))))
    (reduce #'val flags :initial-value 0)))

(defun %nk-begin (context layout title bounds-v4 flags)
  (assert (typep bounds-v4 'rtg-math.types:vec4))
  (assert (stringp title))
  (with-foreign-string (c-title title)
    (nk-begin (pile-nk-ptr context) layout c-title
              (vec4-as-rect bounds-v4)
              (if (numberp flags) flags (window-flags flags)))))

;;------------------------------------------------------------

(defstruct (pile-row-element (:include pile-site-element)))

;; {TODO} Would love to have each body element be a column and check
;;        this stuff, however we cant yet. Because of loops the check
;;        would have to be runtime, which is .. not the best.. still
;;        would be cool though.. hmm
(defmacro in-row-static ((&key (height 30s0) (item-width 100) (cols 1))
                         &body body)
  (with-gensyms (elem)
    `(let ((,elem (make-pile-row-element :name :row-static)))
       (unwind-protect
            (progn
              (pile-push ,elem ,+ctx+)
              (nk-layout-row-static (pile-nk-ptr ,+ctx+) ,height ,item-width ,cols)
              ,@body)
         (pile-pop ,+ctx+)))))

(defmacro in-row ((&key (height 30s0) (cols 1)) &body body)
  (with-gensyms (elem)
    `(let ((,elem (make-pile-row-element :name :row-dynamic)))
       (unwind-protect
            (progn
              (pile-push ,elem ,+ctx+)
              (nk-layout-row-dynamic (pile-nk-ptr ,+ctx+) ,height ,cols)
              ,@body)
         (pile-pop ,+ctx+)))))

;;------------------------------------------------------------

(defmacro button-label (&key text)
  (assert text)
  `(%button-label ,+ctx+ ,text))

(defun %button-label (context text)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (with-foreign-string (c-str text)
    (= 1 (nk-button-label (pile-nk-ptr context) c-str))))

;;------------------------------------------------------------

(defmacro property-int
    (&key text val (min -100) (max 100) (step 2) (inc-per-pixel 1s0))
  (assert text)
  `(%property-int ,+ctx+ ,text ,val ,min ,max ,step ,inc-per-pixel))

(defun %property-int (context text val min max step inc-per-pixel)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (let ((lisp-val (or val min)))
    (with-foreign-string (c-str text)
      (with-foreign-object (val :int)
        (setf (mem-aref val :int) lisp-val)
        (nk-property-int (pile-nk-ptr context) c-str min val max step inc-per-pixel)
        (mem-aref val :int)))))

;;------------------------------------------------------------

(defmacro color-picker (&key (color (cepl:v! 0 0 255 0)) (format :rgba))
  `(%color-picker ,+ctx+ ,color ,format))

(defun %color-picker (context color format)
  (declare (ignore format))
  (assert (typep (pile-head context) 'pile-site-element))
  ;; HMM we really need to be populating these color more efficiently
  color)

;;------------------------------------------------------------

(defmacro render-ui ()
  `(%render-ui ,+ctx+))
