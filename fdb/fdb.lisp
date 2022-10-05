;; -*-lisp-*-
(defpackage :fdb
  (:use :cl :mito :sxql)
  (:use :fdb.app-utils)
  (:export :-main))

(in-package :fdb)

(mito:connect-toplevel :postgres :database-name "marketdb" :username "tradekit" :password "yourpassword")

;; ========================================================================
;; Table defs
;; ========================================================================

(deftable stock ()
  ((id :col-type :bigserial :primary-key t)
   (symbol :col-type :text
           :initarg :symbol
           :accessor stock-symbol)
   (name :col-type :text
         :initarg :name
         :accessor stock-name)
   (exchange :col-type :text
             :initarg exchange
             :accessor stock-exchange)
   (stock-type :col-type (or :text :null)
               :initarg stock-type
               :accessor stock-type)
   (is-etf :col-type :boolean
           :initarg is-etf
           :initform nil
           :accessor stock-is-etf)))

(deftable tick-data ()
  ((dt :col-type :timestamp)
   (open :col-type :numeric)
   (high :col-type :numeric)
   (low :col-type :numeric)
   (close :col-type :numeric)
   (volume :col-type :numeric)
   (stock-id :references (stock id))))

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

