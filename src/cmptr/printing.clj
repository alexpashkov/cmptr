(ns cmptr.printing
  (:require [clojure.string :as str]))

(defn remove-trailing-zeroes [num-str]
  (str/replace num-str #"\.0+$" ""))

(defn print-solutions [phrase solutions]
  (println phrase)
  (doseq [solution (distinct solutions)]
    (println (remove-trailing-zeroes (str solution)))))
