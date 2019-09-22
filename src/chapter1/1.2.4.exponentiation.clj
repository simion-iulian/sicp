(ns chapter1.exponentiation)
(defn square [x] (* x x))
;Recursive exponentiation
(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (dec n)))))

;Linear iteration process
(defn expt-iter [b counter product]
  (if (= counter 0)
    product
    (expt-iter b
               (dec counter)
               (* b product))))

(defn expt-i [b n]
  (expt-iter b n 1))

;Fast exponentiation using linear recursion

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (dec n)))))
(expt 2 2)