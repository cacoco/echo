(ns echo.core
  (:require [cljs.nodejs :as node]
            [clojure.string :as string]
            [echo.util :as util]))

(def http (node/require "http"))
(def utiljs (node/require "util"))

(defn log [data]
  (prn (merge {:ns "echo"} data)))

(def port
  (js/parseInt (util/env "PORT")))

(defn write-response [request response & [data]]
  (let [headers (.headers request)
        callback (get data "callback")
        json (util/to-json
              callback
              (dissoc data "callback"))
        response-headers (-> {} (assoc "Content-Type" "application/json") (assoc "Content-Length" (.length json)))]
    (log {:headers (.inspect utiljs headers "true" "null")})
    (.writeHead response 200 (util/clj->js response-headers))
    (.end response json)))

(defn handler [request response]
  (let [id (util/uid)
        url (.url request)
        query (:query (util/url-parse url))]
    (log {:event "request" :id id :query query})
    (.on request "end" (fn [_]
      (write-response request response query)))
    (.on request "close" (fn [errno]
      (log {:event "close" :errno errno :id id})))))

(defn start [& _]
  (let [server (.createServer http handler)]
    (.listen server port "0.0.0.0")
    (println (str "start port=" port))))

(set! *main-cli-fn* start)