(ns cmptr.core (:require [clojure.string :as str]))

;;$>./computor "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0"
;;Reduced form: 4 * X^0 + 4 * X^1 - 9.3 * X^2 = 0
;;Polynomial degree: 2
;;Discriminant is strictly positive, the two solutions are:
;;0.905239
;;-0.475131

;;$>./computor "5 * X^0 + 4 * X^1 = 4 * X^0"
;;Reduced form: 1 * X^0 + 4 * X^1 = 0
;;Polynomial degree: 1
;;The solution is:
;;-0.25

;;./computor "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0"
;;Reduced form: 5 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 0
;;Polynomial degree: 3
;;The polynomial degree is stricly greater than 2, I can't solve.

(defn split-by-equals-sign [str])

(defn get-term-strings [str]
  (map first (re-seq #"[\+-]?\s?\d+(\.\d+)?\s\*\sX\^\d+" str)))


(defn parse-term [term]
  (let [[_ coef _ deg]
        (re-find #"^([\+-]?\d+(\.\d+)?)\*X\^(\d+)"
                 (str/replace term #"\s" ""))]
    {:coef (Float/parseFloat coef) :deg (Integer/parseInt deg)}))


(defn get-parsed-terms [str]
  (map parse-term (get-term-strings str)))



(defn get-balanced-terms [str]
  (let [[left right] (str/split str #"=")]
    (sort #(compare (:deg %2) (:deg %1))
      (concat (get-parsed-terms left
                                          (map (fn [a] (assoc a :coef (- (:coef a))))
                                            (get-parsed-terms right)))))))


(get-balanced-terms "9.3 * X^4 - 5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")


(defn -main
  [& args]
  (println args))
