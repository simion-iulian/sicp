(ns chapter1.tree-recursion)

;; Fibonacci numbers

(defn fib-recursive [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2)))))

(fib-recursive 30)


(defn fib-iter [a b count]
  

(defn fib-iterative [n]
  (loop [a 1 
         b 0 
         count n]
   (if (zero? count)
    b
    (recur (+ a b) a (dec count)))))
; (time (fib-iterative 33)) much faster than fib-recursive

(defn compare-fib [n]
  (str (fib-recursive n) ":" (fib-iterative n)))
(compare-fib 15)

(fib-recursive 7)
(fib-recursive 6)