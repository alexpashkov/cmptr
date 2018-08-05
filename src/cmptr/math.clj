(ns cmptr.math)

(defn square [x]
  (* x x))

(defn abs [n]
  (max n (- n)))

(defn average [x y]
  (/ (+ x y) 2))

(defn- improve-guess [guess x]
  (average guess (/ x guess)))

(defn- good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.00001))

(defn- sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve-guess guess x) x)))

(defn sqrt [x]
  (float (sqrt-iter 1 x)))
