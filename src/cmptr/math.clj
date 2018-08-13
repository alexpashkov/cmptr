(ns cmptr.math)

(defn mx [& args]                                           ;; there is native implementation, but the task prohibits
  (reduce #(if (> %1 %2) %1 %2) 0 args))

(defn square [x]
  (* x x))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn average [x y]
  (/ (+ x y) 2))

(defn sqrt [x]
  (letfn [(improve-guess [guess x]
            (average guess (/ x guess)))
          (sqrt-iter [guess x]
                     (let [improved-guess (improve-guess guess x)]
                       (if (zero? (- guess improved-guess))
                         guess
                         (recur improved-guess x))))]
    (if (zero? x) 0 (double (sqrt-iter 1 x)))))
