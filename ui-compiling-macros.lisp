(in-package :pile)


(defmacro defui (name args &body body)
  (let ((func-name (intern (format nil "%~a" name) (symbol-package name)))
        (arg-names (mapcar #'first args)))
    `(progn
       ;;
       (defmacro ,name ((&key ,@args))
         (,func-name ,+ctx+ ,@arg-names))
       ;;
       (defun ,func-name (context ,@arg-names)
         ,@body))))
