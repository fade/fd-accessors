;; -*-lisp-*-
;;;; fd-accessors.asd

(asdf:defsystem #:fd-accessors
  :license "GNU Affero General Public License v3 or later, at your discretion."
  :version "0.1.0"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :mailto "fade@deepsky.com"
  :homepage "https://www.deepsky.com/"
  :bug-tracker "https://github.com/fade/doc/issues"
  :source-control (:git "https://github.com/fade/doc")
  :description "get market trading data about companies from various public and private sources. Account registration required in some cases."
  ;; :class :package-inferred-system
  ;; :pathname "src"
  :serial t
  :depends-on (#:DEXADOR
               #:QURI
               #:CL-STRINGS
               #:CL-PPCRE
               #:LOCAL-TIME
               #:CL-JSON-POINTER
               ;; #:JONATHAN
               #:JSOWN
               #:VELLUM
               #:SIMPLE-DATE
               #:SIMPLE-DATE/POSTGRES-GLUE
               #:POSTMODERN
               ;; #:CL-YESQL
               ;; #:CL-YESQL/POSTMODERN
               #:CLOG
               #:PY4CL
               #:DEFMAIN)
  :pathname "./"
  :components ((:file "app-utils")
               (:file "fd-accessors")
               (:file "config")
               (:file "yfinance")))

