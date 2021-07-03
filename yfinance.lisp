(in-package :stonk)

(defclass <yahoo-finance-api> ()
  ((endpoint :initarg :endpoint :initform (error "yahoo-finance-api objects must be created with an appropriate api endpoint URI.")
             :accessor endpoint))
  (:documentation "This object represents the REST endpoint with which
  our local protocol will interact."))

