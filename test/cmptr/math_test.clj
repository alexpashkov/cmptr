(ns cmptr.math-test
  (:require [clojure.test :refer :all]
            [cmptr.math :as math]))

(deftest sqrt
  (testing "Returns a double"
    (is (double? (math/sqrt 0)))
    (is (double? (math/sqrt 1)))
    (is (double? (math/sqrt 10000000)))
    )
  (testing "Returns square roots in double"
    (is (zero? (math/sqrt 0.0)))
    (is (= 1.0 (math/sqrt 1)))
    (is (= 2.0 (math/sqrt 4.0)))
    (is (= 2.0 (math/sqrt 4.0)))
    (is (= 5.0 (math/sqrt 25.0)))
    (is (= 10.0 (math/sqrt 100.0)))
    ))
