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
                :depends-on (#:dexador
                             #:quri
                             #:cl-strings
                             #:cl-ppcre
                             #:local-time
                             #:cl-json-pointer
                             #:jonathan
                             #:jsown
                             ;; #:vellum
                             #:lisp-stat
                             #:simple-date
                             #:simple-date/postgres-glue
                             #:postmodern
                             ;; #:cl-yesql
                             ;; #:cl-yesql/postmodern
                             #:clog
                             ;; #:py4cl
                             #:fdb
                             #:yfinance
                             #:defmain)
                :pathname "./"
                :components ((:file "package")
                             (:file "app-utils")
                             (:file "fd-accessors")
                             (:file "config")))

