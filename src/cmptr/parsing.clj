(ns cmptr.parsing
  (:require [clojure.string :as str]
            [cmptr.math :as math]))

(def term-regex #"[-+]?(((\d+(\.\d+)?)\*(X(\^\d+)?))|((\d+(\.\d+)?))|(X(\^\d+)?))")

(defn- remove-spaces [str] (str/replace str #"\s" ""))

(defn- term-str-is-valid? [term-str]
  (let [[match] (re-matches term-regex term-str)]
    (boolean match)))

(defn- eq-str-is-valid? [eq-str]
  (let [eq-str-without-spaces (remove-spaces eq-str)]
    (every?
      (fn [validator] (apply validator [eq-str-without-spaces]))
      [#(not (empty? %))
       #(= (count (str/split % #"=")) 2)
       #(every? term-str-is-valid? (str/split % #"(=-|=+|((?!^)[\+\-\=]))"))])))

(defn parse-eq-str [eq-str]
  (let [eq-str (remove-spaces eq-str)]
    (if (eq-str-is-valid? eq-str) :ok :not-valid)))

(parse-eq-str "-5 + 1*X^0+4-9.3*X=-1*X^8")
