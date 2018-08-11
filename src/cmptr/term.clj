(ns cmptr.term
  (:require [cmptr.math :as math]))

(defn create [coef deg] {:coef coef :deg deg})

(def test-terms (list (create 1 2) (create 1 2) (create 1 3) (create 1 1000)))

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
       (partition-by get-deg)
       (map sum-terms)
       (flatten)))

(defn group-terms [terms]
  (group-by get-deg terms))

(defn get-max-deg [terms]
  (apply math/max (map get-deg terms)))

(defn reduce-group-terms [terms]
  (reduce-kv
    (fn [acc k v] (assoc acc k (get-coef (sum-terms v))))
    {}
    (group-terms (reduce-terms terms))))

(reduce-group-terms test-terms)
