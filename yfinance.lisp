(in-package :stonk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ticker data objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *user-agent-header* "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36")

(defclass <data-api> ()
  ((base-url :initarg :base-url
             :accessor base-url
             :initform (quri:uri "https://query2.finance.yahoo.com/"))
   (scrape-url :initarg :scrape-url
               :accessor scrape-url
               :initform (quri:uri "https://finance.yahoo.com/quote/")))
  (:documentation "This object represents the REST base-url with which
  our local protocol will interact."))

(defclass <ticker-base> (<data-api>) ;;data-api defaults to yahoo finance.
  ((history :initarg :history :accessor history :initform nil)
   (fundamentals :initarg :fundimentals :accessor fundimentals :initform nil)
   (info :initarg :info :accessor info :initform nil)
   (sustainability :initarg :sustainability :accessor sustainability :initform nil)
   (recommendations :initarg :recommendations :accessor recommendations :initform nil)
   (major-holders :initarg :major-holders :accessor major-holders :initform nil)
   (institutional-holders :initarg :institutional-holders :accessor institutional-holders :initform nil)
   (mutualfund-holders :initarg :mutualfund-holders :accessor mutualfund-holders :initform nil)
   (isin :initarg :isin :accessor isin :initform nil)
   (calendar :initarg :calendar :accessor calendar :initform nil)
   (expirations :initarg :expirations :accessor expirations :initform nil)
   ;; dataframes
   (earnings :initarg :earnings :accessor earnings :initform nil)  ;; (vellum:make-table)
   (financials :initarg :financials :accessor financial :initform nil) ;; (vellum:make-table)
   (balancesheet :initarg :balancesheet :accessor balancesheet :initform nil) ;; (vellum:make-table)
   (cashflow :initarg :cashflow :accessor cashflow :initform nil)) ;; (vellum:make-table)
  (:documentation "This class holds the major data associated with a given tradable asset"))

(defclass <ticker> (<ticker-base>)
  ((ticker :initarg :ticker
           :initform (error "A stock ticker object must be initialised with a ticker symbol. eg. IBM or AAPL."))
   (company :initarg :company
            :initform nil))
  (:documentation "the <ticker> class represents the naming metadata for the symbol and
company representing a tradable asset. Specifically, the company
represented by the market ticker symbol."))

(defclass <ticker-info> (<ticker> <yahoo-finance-api>)
  ((history :initarg :history :initform nil)
   )
  (:documentation "This object contains the data returned from the data provider, parsed and held in vellum dataframes."))

;;========================================================================
;; <ticker-data> methods for object init, and management.
;;========================================================================

(defgeneric write-history (ticker &key period interval start end &allow-other-keys)
  (:documentation "retrieve historical data for a given instance of <ticker> from the
Yahoo Finance API."))

(defmethod write-history ((ticker <ticker>) &key (period "1mo") (interval "1d")
                                              (start nil) (end nil))
  (declare (ignorable period interval start end))
  nil)

;;========================================================================
;; Methods for <ticker> info. Fill it in. Shake it out. Pass it around
;;========================================================================


;; (defgeneric get_data (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_analysts_info (ticker)
;;   (:documentation "Get available analyst info for ticker from "))


;; (defgeneric get_balance_sheet (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_cash_flow (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_day_gainers (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_day_losers (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_day_most_active (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_holders (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_income_statement (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_live_price (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_quote_table (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_top_crypto (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_stats (ticker)
;;   (:documentation "doc"))


;; (defgeneric get_stats_valuation (ticker)
;;   (:documentation "doc"))

;; tickers_dow
;; tickers_nasdaq
;; tickers_other
;; tickers_sp500
