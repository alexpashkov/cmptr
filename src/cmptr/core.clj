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

(defrecord Term [coef deg])


(defn get-term-strings [str]
  (map first (re-seq #"[\+-]?\s?\d+(\.\d+)?\s\*\sX\^\d+" str)))


(defn parse-term [term]
  (let [[_ coef _ deg]
        (re-find #"^([\+-]?\d+(\.\d+)?)\*X\^(\d+)"
                 (str/replace term #"\s" ""))]
    (->Term (Float/parseFloat coef) (Integer/parseInt deg))))


(defn get-parsed-terms [str]
  (map parse-term (get-term-strings str)))


(defn get-balanced-terms [str]
  (let [[left right] (str/split str #"=")]
    (sort #(compare (:deg %2) (:deg %1))
          (concat (get-parsed-terms left)
            (map (fn [a] (assoc a :coef (- (:coef a))))
              (get-parsed-terms right))))))


(defn get-deg [term] (:deg term))

(defn get-coef [term] (:coef term))

(defn validate-same-deg [& terms]
  (when (> (count (distinct (map get-deg terms))) 1) 
    (throw (ex-info "Terms must be of same degree" {}))))

(defn sum-terms 
  ([] (sum-terms (->Term 0 0)))
  ([terms] (validate-same-deg terms
             (->Term (apply + (map get-coef terms))
               (get-deg (first terms))))))



(defn reduce-terms [terms]
  (map sum-terms (partition-by get-deg terms)))


(defn -main
  [str]
  (let [balanced-terms (get-balanced-terms str)]
    balanced-terms))
  

(-main "9.3 * X^4 - 5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")

