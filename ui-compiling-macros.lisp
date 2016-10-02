(in-package :pile)
(in-readtable :fn.reader)

(defmacro defui (name args &body body)
  (let* ((func-name (intern (format nil "%~a" name) (symbol-package name)))
         (arg-names (mapcar λ(first (ensure-list _)) args))
         (m-args (mapcan λ(list (intern (symbol-name _) :keyword) _)
                         arg-names)))
    `(progn
       ;;
       (defmacro ,name ,(when args `(&key ,@args))
         (list ',func-name ',+ctx+ ,@m-args))
       ;;
       (defun ,func-name (,+ctx+ &key ,@args &allow-other-keys)
         ,@body))))
