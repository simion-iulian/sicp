(ns chapter1.sqrt)

(defn square [x] (* x x))

(defn abs [n] (max n (- n)))

(defn average [x y]
  (float (/ (+ x y) 2)))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.0000001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 9)