(in-package :yfinance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ticker data objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *user-agent-header* "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36")

(defclass trading-period ()
  ((timezone :initarg :timezone :initform "GMT" :reader timezone)
   (start-of :initarg :start-of :initform nil :accessor start-of)
   (end-of  :initarg :end-of :initform nil :accessor end-of)
   (gmtoffset :initarg :gmtoffset :initform 0 :accessor gmtoffset)))

(defclass <data-api> ()
  ((base-url :initarg :base-url
             :accessor base-url
             :initform (quri:uri "https://query1.finance.yahoo.com/")))
  (:documentation "This object represents the REST base-url with which
  our local protocol will interact."))

(defclass <yahoo-finance-api>  (<data-api>)
  ((scrape-url :initarg :scrape-url
               :accessor scrape-url
               :initform (quri:uri "https://finance.yahoo.com/quote/"))
   (query-url :initarg :query-url
              :accessor :query-url
              :initform (quri:uri "https://query1.finance.yahoo.com/v11/finance/quoteSummary/{~A}?modules={~A}")))
  (:documentation "This object represents the REST base-url with which
  our local protocol will interact."))

(defparameter *api* (make-instance '<yahoo-finance-api>))

(defclass <ticker-base> ()
  ((history :initarg :history :accessor history :initform nil)
   (pricing-currency :initarg :pricing-currency :accessor pricing-currency :initform nil)
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
   (earnings :initarg :earnings :accessor earnings :initform nil) ;; (vellum:make-table)
   (financials :initarg :financials :accessor financial :initform nil) ;; (vellum:make-table)
   (balancesheet :initarg :balancesheet :accessor balancesheet :initform nil) ;; (vellum:make-table)
   (cashflow :initarg :cashflow :accessor cashflow :initform nil)) ;; (vellum:make-table)
  (:documentation "This class holds the major data associated with a given tradable asset"))

(defclass <ticker> (<ticker-base>)
  ((ticker :initarg :ticker
           :initform (error "A stock ticker object must be initialised with a ticker ticker. eg. IBM or AAPL.")
           :accessor ticker)
   (company :initarg :company
            :initform nil
            :accessor company))
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

;; when a <ticker> is initialised, fill it with data.

(defmethod raw-json ((ticker <ticker>) (apisource <yahoo-finance-api>))
  (let* ((tick (string-upcase (ticker ticker)))
         (url (format nil "~A/v8/finance/chart/~A" (base-url apisource) tick)))
    (multiple-value-bind (body status response-headers uri stream)
        (dex:get url)
      (declare (ignorable body status response-headers uri stream))
      (format t "~&URL: ~A~%" url)
      (let* ((data (jsown:parse body))
             (result (first (jsown:val (jsown:val data "chart") "result")))
             (ticker-info (nthcdr 2 (first (nthcdr 1 result))))
             (timestamps (nthcdr 1 (first (nthcdr 2 result))))
             (indicators (cdr (second (third (first (nthcdr 3 result)))))))
        (values ticker-info data result timestamps indicators)))))

(defun decode-ticker-jsown (ticker &key (api *api*))
  "decode json returned from yahoo finance endpoint. return a ticker object."
  (let* ((tick (string-upcase ticker))
         (url (format nil "~A/v8/finance/chart/~A" (base-url api) tick)))

    (multiple-value-bind (body status response-headers uri stream)
        (dex:get url)
      (declare (ignorable body status response-headers uri stream))
      (format t "~&URL: ~A" url)
      (let* ((raw-data body)
             (data (jsown:parse raw-data))
             (result (first (jsown:val (jsown:val data "chart") "result")))
             (ticker-info (nthcdr 2 (first (nthcdr 1 result))))
             (timestamps (nthcdr 1 (first (nthcdr 2 result))))
             (indicators (cdr (second (third  (first (nthcdr 3 result))))))
             (current-trading-period (first (nthcdr 14 result))))
        (declare (ignorable indicators))
        #|
        TICKER-INFO
        :currency :symbol :exchangeName :instrumentType :firstTradeDate :regularMarketTime
        :gmtoffset :timezone :exhangeTimezoneName :regularMarketPrice :chartPreviousClose
        :previousClose :scale :priceHint
        :currentTradingPeriod ("pre" "regular" "start" "post")
        :dataGranularity :validRanges
        |#
        #|
        TIMESTAMPS
        (list UNIXTIMESTAMPS)
        |#
        (make-instance '<ticker>
                       :ticker (cdr (assoc "symbol" ticker-info :test #'equal))
                       :pricing-currency (cdr (assoc "currency" ticker-info :test #'equal))
                       :exchange-name (cdr (assoc "exchangeName" ticker-info :test #'equal))
                       :instrument-type (cdr (assoc "instrumentType" ticker-info :test #'equal))
                       :first-trade-date (cdr (assoc "firstTradeDate" ticker-info :test #'equal))
                       :regular-market-time (cdr (assoc "regularMarketTime" ticker-info :test #'equal))
                       :gmtoffset (cdr (assoc  "gmtoffset" ticker-info :test #'equal))
                       :timezone (cdr (assoc  "timezone" ticker-info :test #'equal))
                       :exchange-timezone-name (cdr (assoc  "exchangeTimezoneName" ticker-info :test #'equal))
                       :current-trading-period current-trading-period
                       :regular-market-price (cdr (assoc  "regularMarketPrice" ticker-info :test #'equal))
                       :chart-previous-close (cdr (assoc  "chartPreviousClose" ticker-info :test #'equal))
                       :previous-close (cdr (assoc  "previousClose" ticker-info :test #'equal))
                       :scale (cdr (assoc  "scale" ticker-info :test #'equal))
                       :price-hint (cdr (assoc  "priceHint" ticker-info :test #'equal))
                       :timestamps (mapcar #'local-time:unix-to-timestamp timestamps))))))

(defmethod initialize-instance :after ((ticker <ticker>) &rest initargs)
  nil)

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
