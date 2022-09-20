;; -*-lisp-*-
;;;; fdb.asd

(asdf:defsystem #:fdb
  :description "Database storage for the data defined in the fd-accessors suite of APIs to various financial data providers."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  :serial t
  :depends-on (:MITO
               :SXQL
               :CL-STRINGS
               :LOCAL-TIME
               :CHRONICITY)
  :pathname "./"
  :components ((:file "app-utils")
               (:file "config")
               (:file "fdb")))

