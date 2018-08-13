(ns cmptr.core
  (:require [clojure.string :as str]
            [cmptr.parsing :as parsing]
            [cmptr.term :as term]
            [cmptr.solving :refer [solve-eq]]))

(defn remove-trailing-zeroes [num-str]
  (str/replace num-str #"\.0+$" ""))

(defn print-solutions [phrase solutions]
  (println phrase)
  (doseq [solution (distinct solutions)]
    (println (remove-trailing-zeroes (str solution)))))

(defn -main [eq-str]
  (let [reduced-terms (try
                        (term/reduce-terms (parsing/parse-eq eq-str))
                        (catch Exception _ nil))
        max-deg (term/get-max-deg reduced-terms)
        abc (term/get-abc reduced-terms)]
    (if (nil? reduced-terms)
      (println "Incorrect input.")
      (do
        (println (str "Reduced form: " (term/get-formatted-eq-str reduced-terms)))
        (println (str "Polynomial degree: " max-deg))
        (cond
          (zero? max-deg) (println "All real numbers are a solution.")
          (> max-deg 2) (println "The polynomial degree is strictly greater than 2, I can't solve.")
          :else (let [{discriminant :discriminant
                       solutions    :solutions} (apply solve-eq abc)]
                  (cond
                    (= max-deg 1)
                    (print-solutions "The solution is:" solutions)
                    (neg? discriminant)
                    (println "Discriminant is strictly negative, there are no solutions that are real numbers.")
                    (zero? discriminant)
                    (print-solutions "Discriminant is zero, the solution is:" solutions)
                    (pos? discriminant)
                    (print-solutions "Discriminant is strictly positive, the two solutions are:"
                                     solutions))))))))

;(-main "X = X")
;(-main "X=1")
;(-main  "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")
;(-main "5 * X^0 + 4 * X^1 = 4 * X^0")
;(-main "-8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0")
;(-main "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = -3 * X^0 + 8 * X^3")
;(-main "5 + 4 * X + X^2= X^2")
;(-main "--5 + 4.665 * X + X^2 += X^2")
;(-main "+")
;(-main "+++")
;(-main "+%")
