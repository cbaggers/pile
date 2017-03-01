(in-package :pile)
(in-readtable :fn.reader)

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

(defmacro with-ui-context (context &body body)
  `(let ((,+ctx+ ,context))
     ,@body))

(defmacro in-ui (root-element &body body)
  `(with-ui-context (make-context-from-root-element ,root-element)
     (in-input (replay-events (pile-context-root ,+ctx+)))
     (unwind-protect (progn ,@body)
       (nk-clear (pile-nk-ptr ,+ctx+)))))

;;------------------------------------------------------------

(defmacro ui-call (function &rest args)
  `(funcall ,function ,+ctx+ ,@args))

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

(defmacro in-canvas-panel ((&key title (bounds (cepl:v! 50s0 50s0 200s0 250s0))
                                 (flags '(:border :movable :closable :scalable
                                          :minimizable :title)))
                           &body body)
  `(in-panel (:title ,title :bounds ,bounds :flags ,flags)
     (%with-canvas
       ,@body)))

(defmacro %with-canvas (&body body)
  (with-gensyms (canvas)
    `(with-foreign-object (,canvas nk-command-buffer)
       (setf ,canvas (nk-window-get-canvas (pile-nk-ptr ,+ctx+)))
       (unwind-protect
            (progn (pile-push ,canvas ,+ctx+)
                   ,@body)
         (pile-pop ,+ctx+)))))

;; This sucks, make proper readers etc
(defun vec4-as-rect (v4)
  `(raw-bindings-nuklear::h ,(v:w v4)
    raw-bindings-nuklear:w ,(v:z v4)
    raw-bindings-nuklear:y ,(v:y v4)
    raw-bindings-nuklear::x ,(v:x v4)))

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

(defun calc-alignment (alignment)
  (ecase alignment
    (:left nk-text-left)
    (:centered nk-text-centered)
    (:right nk-text-right)))

(defmacro label (&key text (alignment :left))
  (assert text)
  `(%label ,+ctx+ ,text ,alignment))

(defun %label (context text alignment)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (let ((alignment (calc-alignment alignment)))
    (with-foreign-string (c-str text)
      (nk-label (pile-nk-ptr context) c-str alignment))))

;;------------------------------------------------------------

(defmacro text (&key val (alignment :left))
  (assert val)
  `(%text ,+ctx+ ,val ,alignment))

(defun %text (context val alignment)
  (assert (stringp val))
  (assert (typep (pile-head context) 'pile-site-element))
  (with-foreign-string (c-str val)
    (nk-text (pile-nk-ptr context) c-str (length val)
             (calc-alignment alignment))))

;;------------------------------------------------------------

(defmacro text-wrap (&key val)
  (assert val)
  `(%text-wrap ,+ctx+ ,val))

(defun %text-wrap (context val)
  (assert (stringp val))
  (assert (typep (pile-head context) 'pile-site-element))
  (with-foreign-string (c-str val)
    (nk-text-wrap (pile-nk-ptr context) c-str (length val))))

;;------------------------------------------------------------

(defmacro text-input (&key max-length foreign-str
                        str-cursor (nk-filter "nk_filter_default"))
  `(%text-input ,+ctx+ ,max-length ,foreign-str
                ,str-cursor ,nk-filter))

(defun %text-input (context max-length foreign-str
                    str-cursor nk-filter)
  (nk-edit-string (pile-nk-ptr context) nk-edit-field foreign-str
                  str-cursor max-length (cffi:foreign-symbol-pointer nk-filter)))

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

(defmacro option-label (&key text active)
  (assert text)
  `(%option-label ,+ctx+ ,text ,active))

(defun %option-label (context text active)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (with-foreign-string (c-str text)
    (= 1 (nk-option-label (pile-nk-ptr context) c-str (if active 1 0)))))

;;------------------------------------------------------------

(defmacro check-label (&key text active)
  (assert text)
  `(%check-label ,+ctx+ ,text ,active))

(defun %check-label (context text active)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (with-foreign-string (c-str text)
    (= 1 (nk-check-label (pile-nk-ptr context) c-str (if active 1 0)))))

;;------------------------------------------------------------

(defmacro slide-int
    (&key val (min -100) (max 100) (step 2))
  `(%slide-int ,+ctx+ ,val ,min ,max ,step))

(defun %slide-int (context val min max step)
  (assert (typep (pile-head context) 'pile-site-element))
  (let ((lisp-val (floor (or val min))))
    (nk-slide-int (pile-nk-ptr context) min lisp-val max step)))

;;------------------------------------------------------------

(defmacro slide-float
    (&key val (min -100) (max 100) (step 2))
  `(%slide-float ,+ctx+ ,val ,min ,max ,step))

(defun %slide-float (context val min max step)
  (assert (typep (pile-head context) 'pile-site-element))
  (let ((lisp-val (float (or val min))))
    (nk-slide-float (pile-nk-ptr context) min lisp-val max step)))

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

(defmacro property-float
    (&key text val (min -100s0) (max 100s0) (step 2s0) (inc-per-pixel 1s0))
  (assert text)
  `(%property-float ,+ctx+ ,text ,val ,min ,max ,step ,inc-per-pixel))

(defun %property-float (context text val min max step inc-per-pixel)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (let ((lisp-val (float (or val min))))
    (with-foreign-string (c-str text)
      (with-foreign-object (val :float)
        (setf (mem-aref val :float) lisp-val)
        (nk-property-float (pile-nk-ptr context) c-str
                           (float min) val (float max) (float step)
                           inc-per-pixel)
        (mem-aref val :float)))))

;;------------------------------------------------------------

(defmacro property-double
    (&key text val (min -100d0) (max 100d0) (step 2d0) (inc-per-pixel 1s0))
  (assert text)
  `(%property-double ,+ctx+ ,text ,val ,min ,max ,step ,inc-per-pixel))

(defun %property-double (context text val min max step inc-per-pixel)
  (assert text)
  (assert (typep (pile-head context) 'pile-site-element))
  (let ((lisp-val (+ 0d0 (or val min))))
    (with-foreign-string (c-str text)
      (with-foreign-object (val :double)
        (setf (mem-aref val :double) lisp-val)
        (nk-property-double (pile-nk-ptr context) c-str
                            (float min) val (float max) (float step)
                            inc-per-pixel)
        (mem-aref val :double)))))

;;------------------------------------------------------------

(defmacro progress (&key (current 0) (max 100) modifyable)
  `(%progress ,+ctx+ ,current ,max ,modifyable))

(defun %progress (context current max modifyable)
  (assert (typep (pile-head context) 'pile-site-element))
  (with-foreign-object (val :int)
    (setf (mem-aref val :int) (floor current))
    (nk-progress (pile-nk-ptr context) val max
                 (if modifyable 1 0))))

;;------------------------------------------------------------

(defmacro color-picker (&key (color (cepl:v! 0 0 255 0)) (format :rgba))
  `(%color-picker ,+ctx+ ,color ,format))

(defun %color-picker (context color format)
  (assert (typep (pile-head context) 'pile-site-element))
  ;; HMM we really need to be populating these color more efficiently
  (let ((format (ecase format
                  (:rgb nk-rgb)
                  (:rgba nk-rgba)))
        (col (vec4-as-color color)))
    (color-as-vec4 (nk-color-picker (pile-nk-ptr context) col format))))

(defun vec4-as-color (v4)
  `(raw-bindings-nuklear::r
    ,(floor (v:x v4))
    raw-bindings-nuklear:g ,(floor (v:y v4))
    raw-bindings-nuklear:b ,(floor (v:z v4))
    raw-bindings-nuklear::a ,(floor (v:w v4))))


(defun color-as-vec4 (col)
  (cepl:v! (getf col 'raw-bindings-nuklear::r)
           (getf col 'raw-bindings-nuklear::g)
           (getf col 'raw-bindings-nuklear::b)
           (getf col 'raw-bindings-nuklear::a)))

;;------------------------------------------------------------

(defmacro line-graph (&key data (min -1s0) (max 1s0))
  `(%line-graph ,+ctx+ ,data ,min ,max))

(defun %line-graph (context data min max)
  (let ((count (length data))
        (nk-ptr (pile-nk-ptr context)))
    (when (= 1 (nk-chart-begin nk-ptr nk-chart-lines count
                               (float min) (float max)))
      (unwind-protect (map nil λ(nk-chart-push nk-ptr (float _)) data)
        (nk-chart-end nk-ptr)))))

;;------------------------------------------------------------

(defmacro column-graph (&key data (min -1s0) (max 1s0))
  `(%column-graph ,+ctx+ ,data ,min ,max))

(defun %column-graph (context data min max)
  (let ((count (length data))
        (nk-ptr (pile-nk-ptr context)))
    (when (= 1 (nk-chart-begin nk-ptr nk-chart-column count
                               (float min) (float max)))
      (unwind-protect (map nil λ(nk-chart-push nk-ptr (float _)) data)
        (nk-chart-end nk-ptr)))))

;;------------------------------------------------------------

(defmacro render-ui ()
  `(%render-ui ,+ctx+))
