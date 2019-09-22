(ns chapter1.ex1_43)

;nth application
;f(f(f(f(...(f(x))))))

(defn square [x] (* x x))

(defn compose
  [f g]
  (fn [x] (f (g x))))

;1. use recursion
; x|>x+1 f(f...(f(x))) |> x+n
; squaring would be 2^nth power of x
(defn repeated [f n]
  (if (= n 1)
    (fn [x] (f x))
    (compose f (repeated f (dec n)))))

((repeated square 2) 5)
((repeated inc 5) 1)

