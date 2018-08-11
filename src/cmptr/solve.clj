(ns cmptr.solve
  (:require [cmptr.math :as math]))

(defn get-discriminant [a b c]
  (- (* b b) (* 4 (* a c))))

(defn solve [a b c]
  (map
    #(/ (% (- b) (math/sqrt (get-discriminant a b c))) (* 2 a))
    [+ -]))
;(defn remove-trailing-zeroes [num-str]
;  (str/replace num-str #"\.0$" ""))

(defn solve-linear-or-quadratic-eq [[a b c] show-steps]
  (let [discriminant (get-discriminant a b c)
        solutions (if (not (neg? discriminant)) (distinct (solve a b c)) nil)]
    (cond
      (or (zero? a) (zero? discriminant))
      (do
        (println "Discriminant is zero, the solution is:")
        (println (first solutions)))
      (pos? discriminant)
      (do
        (println "Discriminant is strictly positive, the two solutions are:")
        (doseq [solution solutions]
          (println solution))))
      (neg? discriminant)
      (do
        (println "Discriminant is strictly negative, there are no solutions."))))