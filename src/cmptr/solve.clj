(ns cmptr.solve
  (:require [cmptr.math :as math]))

(defn get-discriminant [a b c]
  (- (* b b) (* 4 (* a c))))

(defn solve-eq [a b c]
  (let [discriminant (get-discriminant a b c)]
    {
     :discriminant discriminant
     :solutions    (cond
                     (neg? discriminant) nil
                     (and (zero? a) (zero? b)) ["All real numbers."]
                     (zero? a) [(/ (- c) b)]
                     :else (map
                             #(/ (% (- b) (math/sqrt discriminant)) (* 2 a))
                             [+ -]))
     }))
