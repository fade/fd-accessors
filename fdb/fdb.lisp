;; -*-lisp-*-
(defpackage :fdb
            (:use :cl)
            (:use :fdb.app-utils)
            (:export :-main))

(in-package :fdb)



(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

