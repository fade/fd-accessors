;;;; yfinance.asd

(asdf:defsystem #:yfinance
  :description "Implement the data accessors for Yahoo Finance data API."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license  "GNU AFFERO General Public License."
  :version "0.0.1"
  :serial t
  :depends-on (#:quri
               #:dexador
               #:cl-strings
               #:cl-ppcre
               #:rutils
               #:alexandria)
  
  :components ((:file "package")
               (:file "yfinance")))
