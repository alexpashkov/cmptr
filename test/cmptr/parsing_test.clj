(ns cmptr.parsing-test
  (:require [clojure.test :refer :all]
            [cmptr.parsing :as parsing]))


(def well-formatted-eq-str1  "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")
(def well-formatted-eq-str2 "5 * X^0 + 4 * X^1 = 4 * X^0")
(def well-formatted-eq-str3 "-8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0")
(def well-formatted-eq-str4 "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = -3 * X^0 + 8 * X^3")
(def test-eq-str4 "5 + 4 * X + X^2= X^2")

(parsing/get-term-strs well-formatted-eq-str1)

(deftest get-term-strs
  (testing "returns correct term strings on well formatted equations"
    (is (= (parsing/get-term-strs well-formatted-eq-str1) '(["5*X^0" "+4*X^1" "-9.3*X^2"] ["1*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str2) '(["5*X^0" "+4*X^1"] ["4*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str3) '(["-8*X^0" "-6*X^1" "+0*X^2" "-5.6*X^3"] ["3*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str4) '(["8*X^0" "-6*X^1" "+0*X^2" "-5.6*X^3"] ["-3*X^0" "+8*X^3"])))
    )
  (testing "returns correct term strings on poorly formatted equations"
    (is (= (parsing/get-term-strs test-eq-str4) '(["5" "+4*X" "+X^2"] ["X^2"])))))

