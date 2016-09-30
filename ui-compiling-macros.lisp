(in-package :pile)


(defmacro defui (name args &body body)
  (let* ((func-name (intern (format nil "%~a" name) (symbol-package name)))
         (arg-names (mapcar (lambda (x) (first (ensure-list x))) args)))
    `(progn
       ;;
       (defmacro ,name ,(when args `(&key ,@args))
         (append (list ',func-name ',+ctx+ ,@arg-names)))
       ;;
       (defun ,func-name (,+ctx+ ,@arg-names)
         ,@body))))
