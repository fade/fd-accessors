;; -*-lisp-*-

(defpackage :fd-accessors
  (:use :cl)
  (:use :fd-accessors.app-utils)
  (:export :-main))

(in-package :fd-accessors)

;; Runtime stuff follows.

(defun -main (&optional args)
  (declare (ignorable args))
  (format t "~a~%" "I don't do much yet"))

