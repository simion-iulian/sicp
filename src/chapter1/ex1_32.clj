(ns chapter1.ex1_32)
;Accumulate
;(accumulate combiner null-value term a next b)
;

(defn square [x] (* x x))
(defn accumulator-linear [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
       (accumulator-linear combiner null-value term (next a) next b))))

(defn accumulator-recur [combiner null-value term a next b]
  (loop [a a result null-value]
    (if (> a b)
      result
      (recur (next a) (combiner result (term a))))))

(accumulator-linear + 0 identity 1 inc 3)
(accumulator-recur * 1 identity 1 inc 4)
(reduce * (range 1 5))

(identity +)