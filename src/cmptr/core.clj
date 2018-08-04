(ns cmptr.core
  (:require [clojure.string :as str]))

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
(defn get-deg [^Term term] (:deg term))
(defn get-coef [^Term term] (float (:coef term)))

(defn get-term-strings [str]
  (map first (re-seq #"[\+-]?\s?\d+(\.\d+)?\s\*\sX\^\d+" str)))

(defn parse-term [term]
  (let [[_ coef _ deg]
        (re-find #"^([\+-]?\d+(\.\d+)?)\*X\^(\d+)"
                 (str/replace term #"\s" ""))]
    (->Term (Float/parseFloat coef) (Integer/parseInt deg))))

(defn get-parsed-terms [str]
  (map parse-term (get-term-strings str)))

(defn sort-terms
  ([terms]
   (sort-terms terms >))
  ([terms direction]
   (sort (comparator #(direction (get-deg %1) (get-deg %2))) terms)))

(defn get-moved-left-terms [str]
  (let [[left right] (str/split str #"=")]
    (sort-terms
      (concat (get-parsed-terms left)
              (map (fn [a] (assoc a :coef (- (:coef a))))
                   (get-parsed-terms right))))))

(defn validate-same-deg [& terms]
  (when (> (count (distinct (map get-deg terms))) 1)
    (throw (ex-info "Terms must be of same degree" {}))))

(defn sum-terms
  ([] (sum-terms (->Term 0 0)))
  ([terms]
   (validate-same-deg terms)
   (->Term (apply + (map get-coef terms))
           (get-deg (first terms)))))


(defn reduce-terms [terms]
  (filter #(not (zero? (get-coef %)))
          (map sum-terms (partition-by get-deg terms))))

(defn remove-trailing-zeroes [num-str]
  (str/replace num-str #"\.0$" ""))

(defn get-formatted-eq-str [terms]
  (str/replace (str
                 (str/trim
                   (reduce
                     #(str
                        %1
                        (if (< (get-coef %2) 0) " - " " + ")
                        (str/replace (str (get-coef %2)) #"(-|\.0$)" "")
                        " * X^"
                        (get-deg %2))
                     ""
                     (sort-terms terms <)))
                 " = 0")
               #"^\+\s?" ""))

(defn solve-linear-eq [reduced-terms]
  (let [[variable-term constant-term ] (sort-terms reduced-terms)]
    (println "The solution is:")
    (println (remove-trailing-zeroes
               ( / (get-coef constant-term)
                   (- (get-coef variable-term)))))))

(defn solve-quadratic-eq [reduce-terms] :solve-quadratic-eq)

(defn -main [eq-str]
  (let [reduced-terms (reduce-terms (get-moved-left-terms eq-str))
        [{max-deg :deg}] reduced-terms]
    (println (str "Reduced form: " (get-formatted-eq-str reduced-terms)))
    (println (str "Polynomial degree: " max-deg))
    (cond
      (= 1 max-deg) (solve-linear-eq reduced-terms)
      (= 2 max-deg) (solve-quadratic-eq reduced-terms)
      :else (println "The polynomial degree is strictly greater than 2, I can't solve."))))
