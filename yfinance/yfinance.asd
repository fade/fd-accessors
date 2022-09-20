;;;; yfinance.asd

(asdf:defsystem #:yfinance
  :description "Implement the data accessors for Yahoo Finance data API."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license  "GNU AFFERO General Public License."
  :version "0.0.1"
  :serial t
  :depends-on (#:quri
               #:dexador
               #:jonathan
               #:jsown
               #:cl-strings
               #:cl-ppcre
               #:rutils
               #:alexandria
               #:chronicity
               #:mito)
  
  :components ((:file "package")
               (:file "yfinance")))
