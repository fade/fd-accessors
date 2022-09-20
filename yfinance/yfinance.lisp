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
   (api-url :initarg :api-url
            :accessor api-url
            :initform "")
   (query-url :initarg :query-url
              :accessor query-url
              :initform (quri:uri "https://query1.finance.yahoo.com/v11/finance/quoteSummary/~A?modules=~{~A~^,~}")))
  (:documentation "This object represents the REST base-url with which
  our local protocol will interact."))

(defparameter *api* (make-instance '<yahoo-finance-api>))

(defclass <ticker-base> ()
  ((pricing-currency :initarg :pricing-currency :initform nil :initarg :pcof)
   (exchange-name :initarg :exchange-name :initform nil :accessor exchange-name)
   (instrument-type :initarg :instrument-type :initform nil :accessor instrument-type)
   (first-trade-date :initarg :first-trade-date :initform nil :accessor first-trade-date)
   (regular-market-time :initarg :regular-market-time :initform nil :accessor regular-market-time)
   (gmtoffset :initarg :gmtoffset :initform nil :accessor gmtoffset)
   (timezone :initarg :timezone :initform nil :accessor timezone)
   (exchange-timezone-name :initarg :exchange-timezone-name :initform nil :accessor exchange-timezone-name)
   (regular-market-price :initarg :regular-market-price :initform nil :accessor regular-market-price)
   (chart-previous-close :initarg :chart-previous-close :initform nil :accessor chart-previous-close)
   (previous-close :initarg :previous-close :initform nil :accessor previous-close)
   (scale :initarg :scale :initform nil :accessor scale)
   (price-hint :initarg :price-hint :initform nil :accessor price-hint)
   (current-trading-period :initarg :current-trading-period :initform nil :accessor current-trading-period)
   (trading-periods :initarg :trading-periods :initform nil :accessor trading-periods)
   (data-granularity :initarg :data-granularity :initform nil :accessor data-granularity)
   (range :initarg :range :initform nil :accessor range)
   (valid-ranges :initarg :valid-ranges :initform nil :accessor valid-ranges)
   (timestamps :initarg :timestamps :initform nil :accessor timestamps)
   (indicators :initarg :indicators :initform nil :accessor indicators)
   (period-opens :initarg :period-opens :initform nil :accessor period-opens)
   (period-lows :initarg :period-lows :initform nil :accessor period-lows)
   (period-highs :initarg :period-highs :initform nil :accessor period-highs)
   (period-closes :initarg :period-closes :initform nil :accessor period-closes)
   (period-volumes :initarg :period-volumes :initform nil :accessor period-volumes)
   (period-instant-data :initarg :period-instant-data :initform nil
                        :accessor period-instant-data
                        :documentation "lists of the form: (timestamp open high low close volume)"))
  (:documentation "This class holds the major data associated with a given tradable asset"))

(defclass <ticker> (<ticker-base>)
  ((ticker :initarg :ticker
           :initform (error "A stock ticker object must be initialised with a ticker symbol. eg. IBM or AAPL.")
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
