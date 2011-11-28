(ns echo.util
  (:require [cljs.nodejs :as node]
            [clojure.string :as string]))

(def url (node/require "url"))
(def utiljs (node/require "util"))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.strobj (reduce (fn [m [k v]] (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))

(defn url-parse
  "Returns a map with parsed data for the given URL."
  [u]
  (let [raw (js->clj (.parse url u (clj->js "true")))]
    {:path (get raw "pathname")
     :query (get raw "query")}))

(defn from-json
	"Returns ClojureScript data for the given JSON string."
	[line]
	(js->clj (JSON/parse line)))

(defn to-json
  "Returns a JSON string from the given
   ClojureScript data."
  [callback & data]
  (let [json (str (JSON/stringify (clj->js data)))]
    (if (nil? callback)
      (str json "\n")
      (.format utiljs (str callback "(%s)\n") json))))

(defn random [length]
  "Return a random string on n characters."
  (let [characters (apply vector "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")]
    (loop [acc []]
      (if (= (count acc) length) (apply str acc)
        (recur (conj acc (rand-nth characters)))))))

(defn random-string []
  (string/lower-case (apply str (repeatedly 8 #(random 4)))))

(defn env
  "Returns the value of the environment variable k,
   or raises if k is missing from the environment."
  [k]
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