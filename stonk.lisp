;; -*-lisp-*-

(defpackage :stonk
  (:use :cl)
  (:use :stonk.app-utils)
  (:export :-main))

(in-package :stonk)

;; Runtime stuff follows.

(defun -main (&optional args)
  (declare (ignorable args))
  (format t "~a~%" "I don't do much yet"))

