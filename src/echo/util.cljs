(ns echo.util
  (:require [cljs.nodejs :as node]
            [clojure.string :as string]))

(def url (node/require "url"))
(def utiljs (node/require "util"))

(def alphanumeric "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

(defn randomize [characters length]
  (loop [acc []]
    (if (= (count acc) length) (apply str acc)
      (recur (conj acc (rand-nth characters))))))

(defn uid []
  (string/lower-case (apply str (repeatedly 8 #(randomize alphanumeric 4)))))

(defn clj->js [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.strobj (reduce (fn [m [k v]] (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))

(defn url-parse [u]
  (let [raw (js->clj (.parse url u (clj->js "true")))]
    {:path (get raw "pathname")
     :query (get raw "query")}))

(defn to-json [callback & [data]]
  (let [json (JSON/stringify (clj->js data))]
    (if (nil? callback)
      (str json "\n")
      (.format utiljs "%s(%s)\n" callback json))))

(defn env [k]
  (let [e (js->clj (.env node/process))]
    (or (get e k) (throw (str "Missing Key: " k)))))

(defn trap
  "Trap the Unix signal sig with the given function."
  [sig f]
  (.on node/process (str "SIG" sig) f))

(defn exit
  "Exit with the given status."
  [status]
  (prn status)
  (.exit node/process status))