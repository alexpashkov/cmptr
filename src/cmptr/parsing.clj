(ns cmptr.parsing
  (:require [clojure.string :as str]
            [cmptr.term :as term]))

(def term-regex #"[-+]?(((\d+(\.\d+)?)\*?(X(\^\d+)?))|((\d+(\.\d+)?))|(X(\^\d+)?))")

(defn- remove-spaces [str] (str/replace str #"\s" ""))

(defn- get-term-strs [eq-str]
  (map #(str/split % #"(?=[\+\-])") (str/split (remove-spaces eq-str) #"=")))

(defn- term-str-is-valid? [term-str]
  (let [[match] (re-matches term-regex term-str)]
    (boolean match)))

(defn- eq-str-is-valid? [eq-str]
  (let [eq-str-without-spaces (remove-spaces eq-str)]
    (every?
      (fn [validator] (apply validator [eq-str-without-spaces]))
      [#(not (empty? %))
       #(= (count (str/split % #"=")) 2)
       #(every? term-str-is-valid? (flatten (get-term-strs %)))])))

(defn parse-term [term-str]
  (let [[_ coef _ _ X _ deg] (re-matches #"^(([-+])?\d+(\.\d+)?)?(\*?X(\^(\d+))?)?$" term-str)]
    (or (term-str-is-valid? term-str)
        (throw (ex-info "Term string is not valid." {})))
    {:coef (float (Float/parseFloat (if (nil? coef) 1 coef)))
     :deg  (Integer/parseInt (if X (if deg deg "1") "0"))}))

(defn parse-eq [eq-str]
  (let [eq-str (remove-spaces eq-str)]
    (or (eq-str-is-valid? eq-str) (throw (ex-info "Equation string is not valid." {})))
    (let [[left right] (map #(map parse-term %) (get-term-strs eq-str))]
      (concat left (map term/complement-coef-sign right)))))

