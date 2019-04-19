(ns chapter1.recursion-iteration)

(defn recursive-factorial [n]
  (if (= n 1)
    1
    (* n (recursive-factorial (- n 1)))))

(recursive-factorial 4)

(defn fact-iter [accumulator counter maxcount]
  (if (> counter maxcount)
    accumulator
    (fact-iter (* counter accumulator)
               (+ counter 1)
               maxcount)))

(defn iterative-factorial [n]
  (fact-iter 1 1 n))

(iterative-factorial 20)
 
(defn plus [a b]
  (if (= a 0)
    b
    (inc (plus (dec a) b))))

(plus 1 5)

