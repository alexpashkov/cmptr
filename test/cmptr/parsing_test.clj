(ns cmptr.parsing-test
  (:require [clojure.test :refer :all]
            [cmptr.parsing :as parsing]))


(def well-formatted-eq-str1  "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0")
(def well-formatted-eq-str2 "5 * X^0 + 4 * X^1 = 4 * X^0")
(def well-formatted-eq-str3 "-8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0")
(def well-formatted-eq-str4 "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = -3 * X^0 + 8 * X^3")
(def test-eq-str4 "5 + 4 * X + X^2= X^2")
(def test-eq-str5 "--5 + 4.665 * X + X^2 += X^2")

(parsing/get-term-strs well-formatted-eq-str1)

(deftest get-term-strs
  (testing "returns correct term strings on well formatted equations"
    (is (= (parsing/get-term-strs well-formatted-eq-str1) '(["5*X^0" "+4*X^1" "-9.3*X^2"] ["1*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str2) '(["5*X^0" "+4*X^1"] ["4*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str3) '(["-8*X^0" "-6*X^1" "+0*X^2" "-5.6*X^3"] ["3*X^0"])))
    (is (= (parsing/get-term-strs well-formatted-eq-str4) '(["8*X^0" "-6*X^1" "+0*X^2" "-5.6*X^3"] ["-3*X^0" "+8*X^3"])))
    )
  (testing "returns correct term strings on poorly formatted equations"
    (is (= (parsing/get-term-strs test-eq-str4) '(["5" "+4*X" "+X^2"] ["X^2"]))))

  (testing "returns correct term strings on incorrectly formatted equations"
    (is (= (parsing/get-term-strs test-eq-str5) '(["-" "-5" "+4.665*X" "+X^2" "+"] ["X^2"]))))
  )

(deftest term-str-is-valid?
  (testing "returns true for valid term strings"
    (testing "containing only coefficients"
      (is (true? (parsing/term-str-is-valid? "8")))
      (is (true?(parsing/term-str-is-valid? "-8")))
      (is (true? (parsing/term-str-is-valid? "-8.5")))
      (is (true? (parsing/term-str-is-valid? "-0000")))
      )
    (testing "containing only X without coefficient with and without degree"
      (is (true? (parsing/term-str-is-valid? "X")))
      (is (true? (parsing/term-str-is-valid? "X^0")))
      (is (true? (parsing/term-str-is-valid? "X^3")))
      )
    (testing "containing X without degree"
      (is (true? (parsing/term-str-is-valid? "-0*X")))
      (is (true? (parsing/term-str-is-valid? "-0.5*X")))
      (is (true? (parsing/term-str-is-valid? "0.5*X")))
      (is (true? (parsing/term-str-is-valid? "5*X")))
      (is (true? (parsing/term-str-is-valid? "5.555*X")))
      )
    (testing "containing coefficient with X with degree"
      (is (true? (parsing/term-str-is-valid? "-0*X^2")))
      (is (true? (parsing/term-str-is-valid? "-0.5*X^3")))
      (is (true? (parsing/term-str-is-valid? "0.5*X^1")))
      (is (true? (parsing/term-str-is-valid? "5*X^0")))
      (is (true? (parsing/term-str-is-valid? "5.555*X^3")))
      )
    ))
