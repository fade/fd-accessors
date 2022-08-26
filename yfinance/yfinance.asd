;;;; yfinance.asd

(asdf:defsystem #:yfinance
  :description "Describe yfinance here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fd-accessors #:quri #:dexador #:cl-strings #:rutils #:alexandria)
  :components ((:file "package")
               (:file "yfinance")))
