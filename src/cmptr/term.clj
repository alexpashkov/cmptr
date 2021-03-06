(ns cmptr.term
  (:require [clojure.string :as str]
            [cmptr.math :as math]
            [cmptr.printing :refer [remove-trailing-zeroes]]))


(defn create [coef deg] {:coef coef :deg deg})

(defn get-coef [term] (get term :coef))

(defn get-deg [term] (get term :deg))

(defn set-coef [term v] (assoc term :coef v))

(defn complement-coef-sign [term] (set-coef term (- (get-coef term))))

(defn same-deg? [terms]
  (= (count (distinct (map get-deg terms))) 1))

(defn sum-terms [terms]
  (when-not (empty? terms)
    (or (same-deg? terms) (throw (ex-info "Summed up terms must be of same deg" {})))
    (create (reduce #(+ % (get-coef %2)) 0 terms) (get-deg (first terms)))))

(defn reduce-terms [terms]
  (->> terms
       (group-by get-deg)
       (vals)
       (map sum-terms)
       (flatten)
       (filter #(not (zero? (get-coef %))))))

(defn group-terms [terms]
  (group-by get-deg terms))

(defn get-max-deg [terms]
  (apply math/mx (map get-deg terms)))

(defn reduce-group-terms [terms]
  (reduce-kv
    (fn [acc k v] (assoc acc k (get-coef (sum-terms v))))
    {}
    (group-terms (reduce-terms terms))))

(defn get-abc [terms]
  (let [{a   2 b 1 c 0
         :or {a 0 b 0 c 0}} (reduce-group-terms terms)]
    [a b c]))

(defn sort-terms
  ([terms]
   (sort-terms terms >))
  ([terms direction]
   (sort (comparator #(direction (get-deg %1) (get-deg %2))) terms)))

(defn get-formatted-eq-str [terms]
  (letfn [(get-coef-str [{coef :coef deg :deg}]
            (str (if (pos? coef) " + " " - ")
                 (if (and (= 1.0 (math/abs coef)) (not (zero? deg)))
                   ""
                   (remove-trailing-zeroes
                     (str (math/abs coef))))))
          (get-X-str [{coef :coef deg :deg}]
            (if (zero? deg)
              ""
              (str
                (if (or (zero? deg) (= 1.0 (math/abs coef))) "" " * ")
                "X" (if (= 1 deg) "" (str "^" deg)))))]
    (-> (if (empty? terms)
          "0 = 0"
          (str (reduce (fn [acc-str term]
                         (str
                           acc-str
                           (get-coef-str term) (get-X-str term)))
                       ""
                       (sort-terms terms <)) " = 0"))
        (str/replace #"^\s*\+?(-)?\s*" "$1"))))
