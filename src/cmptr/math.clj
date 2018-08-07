(ns cmptr.math)

(defn square [x]
  (* x x))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn average [x y]
  (/ (+ x y) 2))

(defn sqrt [x]
  (letfn [(good-enough? [guess x]
            (< (abs (- (square guess) x)) 0.00000000001))

          (improve-guess [guess x]
                         (average guess (/ x guess)))

          (sqrt-iter [guess x]
                     (if (good-enough? guess x)
                       guess
                       (recur (improve-guess guess x) x)))]
    (if (zero? x) 0 (float (sqrt-iter 1 x)))))
