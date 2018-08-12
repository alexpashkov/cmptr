(ns cmptr.parsing-test
  (:require [clojure.test :refer :all]
            [cmptr.parsing :as parsing]))

(def well-formatted-eq-str1  "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")
(def well-formatted-eq-str2 "5 * X^0 + 4 * X^1 = 4 * X^0")
(def well-formatted-eq-str3 "-8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0")
(def well-formatted-eq-str4 "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = -3 * X^0 + 8 * X^3")
(def test-eq-str4 "5 + 4 * X + X^2= X^2")
(def test-eq-str5 "--5 + 4.665 * X + X^2 += X^2")

(deftest get-term-strs
  (testing "returns correct term strings on well formatted equations"
    (is (= (parsing/get-term-strs well-formatted-eq-str1) '(["5*X^0" "+4*X^1" "-9.3*X^2"] ["1*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str2) '(["5*X^0" "+4*X^1"] ["4*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str3) '(["-8*X^0" "-6*X^1" "+0*X^2" "-5.6*X^3"] ["3*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str4) '(["8*X^0" "-6*X^1" "+0*X^2" "-5.6*X^3"] ["-3*X^0" "+8*X^3"]))))
  (testing "returns correct term strings on poorly formatted equations"
    (is (= (parsing/get-term-strs test-eq-str4) '(["5" "+4*X" "+X^2"] ["X^2"]))))

  (testing "returns correct term strings on incorrectly formatted equations"
    (is (= (parsing/get-term-strs test-eq-str5) '(["-" "-5" "+4.665*X" "+X^2" "+"] ["X^2"])))))

(deftest term-str-is-valid?
  (testing "returns true for valid term strings"
    (testing "containing only coefficients"
      (is (true? (parsing/term-str-is-valid? "8")))
      (is (true? (parsing/term-str-is-valid? "-8")))
      (is (true? (parsing/term-str-is-valid? "-8.5")))
      (is (true? (parsing/term-str-is-valid? "-0000"))))
    (testing "containing only X without coefficient with and without degree"
      (is (true? (parsing/term-str-is-valid? "X")))
      (is (true? (parsing/term-str-is-valid? "X^0")))
      (is (true? (parsing/term-str-is-valid? "X^3"))))
    (testing "containing X without degree"
      (is (true? (parsing/term-str-is-valid? "-0*X")))
      (is (true? (parsing/term-str-is-valid? "-0.5*X")))
      (is (true? (parsing/term-str-is-valid? "0.5*X")))
      (is (true? (parsing/term-str-is-valid? "5*X")))
      (is (true? (parsing/term-str-is-valid? "5.555*X"))))
    (testing "containing coefficient with X with degree"
      (is (true? (parsing/term-str-is-valid? "-0*X^2")))
      (is (true? (parsing/term-str-is-valid? "-0.5*X^3")))
      (is (true? (parsing/term-str-is-valid? "0.5*X^1")))
      (is (true? (parsing/term-str-is-valid? "5*X^0")))
      (is (true? (parsing/term-str-is-valid? "5.555*X^3")))

      (testing "without star"
        (is (true? (parsing/term-str-is-valid? "-0X^2")))
        (is (true? (parsing/term-str-is-valid? "-0.5X^3")))
        (is (true? (parsing/term-str-is-valid? "0.5X^1")))
        (is (true? (parsing/term-str-is-valid? "5X^0")))
        (is (true? (parsing/term-str-is-valid? "5.555X^3"))))))

  (testing "returns false for invalid term strings"
    (is (false? (parsing/term-str-is-valid? "")))
    (is (false? (parsing/term-str-is-valid? " ")))
    (is (false? (parsing/term-str-is-valid? "+")))
    (is (false? (parsing/term-str-is-valid? "-")))
    (is (false? (parsing/term-str-is-valid? "=")))
    (is (false? (parsing/term-str-is-valid? "*")))
    (is (false? (parsing/term-str-is-valid? "^")))
    (is (false? (parsing/term-str-is-valid? "8*")))
    (is (false? (parsing/term-str-is-valid? "-8*")))
    (is (false? (parsing/term-str-is-valid? "*X")))
    (is (false? (parsing/term-str-is-valid? "X^")))
    (is (false? (parsing/term-str-is-valid? "*X^")))
    (is (false? (parsing/term-str-is-valid? "*X^2")))
    (is (false? (parsing/term-str-is-valid? "8X^")))
    (is (false? (parsing/term-str-is-valid? "8X^")))
    (is (false? (parsing/term-str-is-valid? "^2")))))

(deftest parse-term
  (testing "returns terms maps for valid term strings"
    (is (= (parsing/parse-term "8*X^0") {:coef 8.0 :deg 0}))
    (is (= (parsing/parse-term "8*X^1") {:coef 8.0 :deg 1}))
    (is (= (parsing/parse-term "8.5*X^1") {:coef 8.5 :deg 1}))
    (is (= (parsing/parse-term "8") {:coef 8.0 :deg 0}))
    (is (= (parsing/parse-term "8*X") {:coef 8.0 :deg 1}))
    (is (= (parsing/parse-term "8.555*X") {:coef 8.555 :deg 1}))
    (is (= (parsing/parse-term "0.555*X") {:coef 0.555 :deg 1}))
    (is (= (parsing/parse-term "8.3335*X") {:coef 8.3335 :deg 1}))
    (is (= (parsing/parse-term "X") {:coef 1.0 :deg 1}))
    (is (= (parsing/parse-term "X^0") {:coef 1.0 :deg 0}))
    (is (= (parsing/parse-term "X^3") {:coef 1.0 :deg 3}))
    (is (= (parsing/parse-term "0*X^3") {:coef 0.0 :deg 3}))
    (is (= (parsing/parse-term "3*X^3") {:coef 3.0 :deg 3}))
    (is (= (parsing/parse-term "0X^3") {:coef 0.0 :deg 3}))
    (is (= (parsing/parse-term "3X^3") {:coef 3.0 :deg 3})))
  (testing "throws Exception for invalid term strings"
    (is (thrown? Exception (parsing/parse-term "")))
    (is (thrown? Exception (parsing/parse-term "XX")))
    (is (thrown? Exception (parsing/parse-term "+")))))
