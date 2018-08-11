(ns cmptr.core
  (:require [clojure.string :as str]
            [cmptr.parsing :as parsing]
            [cmptr.math :as math]
            [cmptr.term :as term]))

;(parsing/parse-eq "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")
;;Reduced form: 4 * X^0 + 4 * X^1 - 9.3 * X^2 = 0
;;Polynomial degree: 2
;;Discriminant is strictly positive, the two solutions are:
;;0.905239
;;-0.475131

;(parsing/parse-eq "5 * X^0 + 4 * X^1 = 4 * X^0")
;;$>./computor
;;Reduced form: 1 * X^0 + 4 * X^1 = 0
;;Polynomial degree: 1
;;The solution is:
;;-0.25

;(parsing/parse-eq "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0")
;;./computor "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0"
;;Reduced form: 5 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 0
;;Polynomial degree: 3
;;The polynomial degree is stricly greater than 2, I can't solve.

;(defrecord Term [coef deg])
;(defn get-deg [^Term term] (:deg term))
;(defn get-coef [^Term term] (if (nil? term) 0 (float (:coef term))))

;(parsing/parse-eq "5 * X^0 + 4*X^1= 999 + 150*X^2")


;(defn eq-str-is-valid? [eq-str]
;  (let [parts (str/split (remove-spaces eq-str) #"=")]
;    (if (= (count parts) 2)
;      parts
;      false)))


;
;
;(defn parse-eq [eq-str]
;  (try
;    (->> eq-str
;         (get-term-strs)
;         (map parse-term))
;    (catch Exception e nil)))
;
;(map first (re-seq term-regex "8*X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0"))
;
;
;;(defn get-term-strings [str]
;;  (map first (re-seq #"[\+-]?\s?\d+(\.\d+)?\s\*\sX\^\d+" str)))
;;
;;(defn parse-term [term]
;;  (let [[_ coef _ deg]
;;        (re-find #"^([\+-]?\d+(\.\d+)?)\*X\^(\d+)"
;;                 (str/replace term #"\s" ""))]
;;    (->Term (Float/parseFloat coef) (Integer/parseInt deg))))
;
;
;
;(defn get-parsed-terms [str]
;  (map parse-term (get-term-strings str)))
;
;(defn get-moved-left-terms [str]
;  (let [[left right] (str/split str #"=")]
;    (sort-terms
;      (concat (get-parsed-terms left)
;              (map (fn [a] (assoc a :coef (- (:coef a))))
;                   (get-parsed-terms right))))))
;
;
;(defn sum-terms
;  ([] (sum-terms (->Term 0 0)))
;  ([terms]
;   (validate-same-deg terms)
;   (->Term (apply + (map get-coef terms))
;           (get-deg (first terms)))))
;
;(defn reduce-terms [terms]
;  (group-by get-deg
;            (filter
;              #(not (zero? (get-coef %)))
;              (map sum-terms (partition-by get-deg terms)))))
;
;(def test-terms (->> (range 0 3)
;                     (map #(->Term (inc %) %))))
;
;(defn remove-trailing-zeroes [num-str]
;  (str/replace num-str #"\.0$" ""))
;
;;; TODO refactor
;(defn get-formatted-eq-str [terms]
;  (str/replace (str
;                 (str/trim
;                   (reduce
;                     #(str
;                        %1
;                        (if (< (get-coef %2) 0) " - " " + ")
;                        (str/replace (str (get-coef %2)) #"(-|\.0$)" "")
;                        " * X^"
;                        (get-deg %2))
;                     ""
;                     (sort-terms (flatten (vals terms)) <)))
;                 " = 0")
;               #"^\+\s?"
;               ""))
;
;
;
;(defn print-solution [str]
;  (println "The solution is:")
;  (println str))
;
;
;(defn solve-linear-eq [reduced-terms]
;  (let [{[variable-term] 1 [constant-term] 0} reduced-terms]
;    (print-solution
;      (remove-trailing-zeroes
;        (/ (get-coef constant-term)
;           (- (get-coef variable-term)))))))
;
;(defn get-abc [terms]
;  (let [{[{a :coef}] 2 [{b :coef}] 1 [{c :coef}] 0} terms]
;    (map #(if (nil? %) 0 %) [a b c])))
;
;(defn solve-quadratic-eq [reduced-terms]
;  (letfn [(get-discriminant [a b c]
;            (- (* b b) (* 4 (* a c))))
;          (solve [a b c]
;            (map
;              #(/ (% (- b) (math/sqrt (get-discriminant a b c))) (* 2 a))
;              [+ -]))]
;    (let [[a b c] (get-abc reduced-terms)
;          discriminant (get-discriminant a b c)
;          solutions (if (not (neg? discriminant)) (distinct (solve a b c)) nil)]
;      (cond
;        (neg? discriminant)
;        (do
;          (println "Discriminant is strictly negative, there are no solutions."))
;        (zero? discriminant)
;        (do
;          (println "Discriminant is zero, the solution is:")
;          (println (first solutions)))
;        (pos? discriminant)
;        (do
;          (println "Discriminant is strictly positive, the two solutions are:")
;          (doseq [solution solutions]
;            (println solution)))))))
;
;(defn get-terms-max-deg [terms]
;  (if (empty? terms) 0 (first (sort > (keys terms)))))
;
(defn -main [eq-str]
  (let [terms (try
                (parsing/parse-eq eq-str)
                (catch Exception _ nil))
        reduced-terms (term/reduce-terms terms)]
    (if (nil? terms)
      (println "Incorrect input.")
      (let [solution (apply math/solve-eq (term/get-abc terms))]
        (println solution)))))
;(defn -main [eq-str]
;  (let [reduced-terms (reduce-terms (get-moved-left-terms eq-str))
;        max-deg (get-terms-max-deg reduced-terms)]
;    (println (str "Reduced form: " (get-formatted-eq-str reduced-terms)))
;    (println (str "Polynomial degree: " max-deg))
;    (cond
;      (= 0 max-deg) (print-solution "All real numbers.")
;      (= 1 max-deg) (solve-linear-eq reduced-terms)
;      (= 2 max-deg) (solve-quadratic-eq reduced-terms)
;      :else (println "The polynomial degree is strictly greater than 2, I can't solve."))))
