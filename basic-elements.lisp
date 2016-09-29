(in-package :pile)

(defvar +ctx+ 'nk-ctx)

;;------------------------------------------------------------

(defmacro with-context (root-element &body body)
  `(let ((,+ctx+ (make-context-from-root-element ,root-element)))
     ,@body))

;;------------------------------------------------------------

;; what if not =1? silent fail is ugly
(defmacro in-input (&body body)
  `(when (= 1 (nk-input-begin (pile-root ,+ctx+)))
     (unwind-protect (progn ,@body)
       (nk-input-end (pile-root ,+ctx+)))))

;;------------------------------------------------------------

;; {NOTE} this was in-layout but we are making an nk-panel and so is renamed
;;        for now. Can anything else be the layout?
;;
;; what if not =1? silent fail is ugly
(defmacro in-panel ((&key title (bounds (cepl:v! 50s0 50s0 200s0 250s0))
                          (flags '(:border :movable :closable :scalable
                                   :minimizable :title)))
                    &body body)
  (assert (stringp title))
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
               (nk-end (pile-root ,+ctx+)))))))))

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
  (with-foreign-string (c-title title)
    (nk-begin (pile-root context) layout c-title
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
              (nk-layout-row-static nk-ctx ,height ,item-width ,cols)
              ,@body)
         (pile-pop ,+ctx+)))))

(defmacro in-row-dynamic ((&key (height 30s0) (cols 1)) &body body)
  (with-gensyms (elem)
    `(let ((,elem (make-pile-row-element :name :row-dynamic)))
       (unwind-protect
            (progn
              (pile-push ,elem ,+ctx+)
              (nk-layout-row-dynamic nk-ctx ,height ,cols)
              ,@body)
         (pile-pop ,+ctx+)))))

;;------------------------------------------------------------

(defmacro button-label (&key text)
  (assert text)
  `(%button-label ,+ctx+ ,text))

(defun %button-label (context text)
  (assert text)
  (assert (typep (pile-peek context) 'pile-site-element))
  (with-foreign-string (c-str text)
    (nk-button-label (pile-root context) c-str)))

;;------------------------------------------------------------

(defmacro property-int
    (&key text (min -100) (max 100) (step 2) (inc-per-pixel 1s0))
  (assert text)
  `(%property-int ,+ctx+ ,text ,min ,max ,step ,inc-per-pixel))

(defun %property-int (context text min max step inc-per-pixel)
  (assert text)
  (assert (typep (pile-peek context) 'pile-site-element))
  (with-foreign-string (c-str text)
    (with-foreign-object (val :int)
      (nk-property-int (pile-root context) c-str min val max step inc-per-pixel)
      (mem-aref val :int))))

;;------------------------------------------------------------

(defmacro color-picker (&key (color (cepl:v! 0 0 255 0)) (format :rgba))
  `(%color-picker ,+ctx+ ,color ,format))

(defun %color-picker (context color format)
  (assert (typep (pile-peek context) 'pile-site-element))
  ;; HMM we really need to be populating these color more efficiently
  color)
