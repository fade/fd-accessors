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

(defun make-ticker (symbol)
  (make-instance '<ticker> :ticker symbol))

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


(defmethod initialize-instance :after ((tick <ticker>) &key)
  "Turn the tiemstamps returned from yahoo finance into local-time timestamps which we can use for date-math."
  (setf (first-trade-date tick) (local-time:unix-to-timestamp (first-trade-date tick))
        (regular-market-time tick) (local-time:unix-to-timestamp (regular-market-time tick))
        (current-trading-period tick) (make-instance 'trading-period
                                                     :timezone (gethash "timezone" (current-trading-period tick))
                                                     :start-of (local-time:unix-to-timestamp (gethash "start" (current-trading-period tick)))
                                                     :end-of (local-time:unix-to-timestamp (gethash "end" (current-trading-period tick)))
                                                     :gmtoffset (gethash "gmtoffset" (current-trading-period tick)))
        (period-instant-data tick) (mapcar #'list (timestamps tick) (period-opens tick)
                                           (period-highs tick) (period-lows tick) (period-closes tick)
                                           (period-volumes tick))))

(defun parse-ticker-as-hash (body)
  "Given a JSON body, parse it with jonathan, returning a hash-table."
  (jonathan:parse body :as :hash-table :junk-allowed t))

(defun get-chart (ticker &optional start end)
  (declare (ignorable start end))
  (let* ((tick (string-upcase ticker))
         (url (format nil "~A/v8/finance/chart/~A" (quri:render-uri (base-url *api*)) tick)))
    (format t "~&~A~%" url)
    (multiple-value-bind (body status response-headers uri stream)
        (dex:get url)
      (declare (ignorable body status response-headers uri stream))
      (parse-ticker-as-hash body))))

(defun decode-ticker (ticker &optional start end)
  (declare (ignorable start end))
  (let* ((tick (string-upcase ticker))  
         (url (format nil "~A/v8/finance/chart/~A" (quri:render-uri (base-url *api*)) tick)))

    (multiple-value-bind (body status response-headers uri stream)
        (dex:get url)
      (declare (ignorable body status response-headers uri stream))
      (format t "~&[[~A]]" url)

      (let* ((data (parse-ticker-as-hash body))
             (result (first (gethash "result" (gethash "chart" data))))
             (ticker-info (gethash "meta" result)) 
             (timestamps (gethash "timestamp" result)) ;; this is a list
             (indicators (first (gethash "quote" (gethash "indicators" result))))
             (current-trading-period (gethash "currentTradingPeriod" ticker-info))) ;; post / pre / regular

        (values
         (make-instance '<ticker>
                        :ticker (gethash "symbol" ticker-info)
                        :pricing-currency (gethash "currency" ticker-info)
                        :exchange-name (gethash "exchangeName" ticker-info)
                        :instrument-type (gethash "instrumentType" ticker-info)
                        :first-trade-date (gethash "firstTradeDate" ticker-info)
                        :regular-market-time (gethash "regularMarketTime" ticker-info)
                        :gmtoffset (gethash "gmtoffset" ticker-info)
                        :timezone (gethash "timezone" ticker-info)
                        :valid-ranges (gethash "validRanges" ticker-info)
                        :exchange-timezone-name (gethash "exchangeTimezoneName" ticker-info)
                        :current-trading-period (gethash "regular" current-trading-period)
                        :regular-market-price (gethash  "regularMarketPrice" ticker-info)
                        :chart-previous-close (gethash "chartPreviousClose" ticker-info)
                        :previous-close (gethash "previousClose" ticker-info)
                        :scale (gethash "scale" ticker-info)
                        :price-hint (gethash "priceHint" ticker-info)
                        :timestamps (mapcar #'local-time:unix-to-timestamp timestamps)
                        :indicators indicators
                        :period-closes (gethash "close" indicators)
                        :period-lows (gethash "low" indicators)
                        :period-highs (gethash "high" indicators)
                        :period-opens (gethash "open" indicators)
                        :period-volumes (gethash "volume" indicators)))))))

(defun get-arbitrary-thing (symbol thinglist)
  (let* ((url (format nil (quri:render-uri (query-url *api*))
                      (string-upcase symbol) thinglist)))
    (multiple-value-bind (body status response-headers uri stream)
        (dex:get url)
      (declare (ignorable body status response-headers uri stream))
      (format t "~&URL: ~A~%" url)
      (let* ((data (jsown:parse body)))
        (values data)))))


;;========================================================================
;; Methods for <ticker> info. Fill it in. Shake it out. Pass it around
;;========================================================================

(defgeneric write-history (ticker &key period interval start end &allow-other-keys)
  (:documentation "retrieve historical data for a given instance of <ticker> from the
Yahoo Finance API."))

(defmethod write-history ((ticker <ticker>) &key (period "1mo") (interval "1d")
                                              (start nil) (end nil))
  (declare (ignorable period interval start end))
  nil)

;; ------------------------------------------------------------------------ 

(defgeneric asset_profile (ticker api)
  (:documentation "The assetProfile contains general information about the company,
such as industy, full time employees, and its website and long business summary. This data
should provide the 200 meter overview of a company."))

(defmethod asset_profile ((ticker <ticker>) (apisource <yahoo-finance-api>))
  (call-next-method))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_analysts_info (ticker)
;;   (:documentation "Get available analyst info for ticker from "))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_balance_sheet (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_cash_flow (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_day_gainers (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_day_losers (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_day_most_active (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_holders (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_income_statement (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_live_price (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_quote_table (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_top_crypto (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_stats (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; (defgeneric get_stats_valuation (ticker)
;;   (:documentation "doc"))

;; ------------------------------------------------------------------------ 

;; tickers_dow
;; tickers_nasdaq
;; tickers_other
;; tickers_sp500
