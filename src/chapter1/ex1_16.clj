(ns chapter1.ex1_16)

(defn square [x] (* x x))

;Fast exponentiation using linear recursion
; even? -> b^n = (b^(n/2))^2 = (b^2)^(n/2)
; odd? -> b^n = b* (b^((n - 1)/2))^2 = b*(b^2)^((n-1)/2)
; b^6 = (b^3)^2 = (b^2)^3
; b^n = 
(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (dec n)))))

;Linear iteration process
(defn expt-iter [b counter product]
  (if (= counter 0)
    product
    (expt-iter b
               (dec counter)
               (* b product))))

(defn expt-i [b n]
  (expt-iter b n 1))


(defn exp-red-log [b n]
  (cond (= n 0) 1
    (even?)
    (reduce * (repeat (/ n 2) (square b)))
    :else (* b (reduce * (repeat (/ n 2) (square b))))))

(defn exp-red [b n]
  (reduce * (repeat n b)))


(defn state-expt [a b n]
  (prn a "-" b "-" n)
  (cond (= n 0) a
        (even? n) (state-expt (* a (square b)) b (/ n 2))
        :else (state-expt (* a  b) b (dec n))))

(defn expt-s [b n]
  (if (even? n)
    (state-expt 1 b n)
    (state-expt b b n)))

(exp-red 2 8)
(expt-s 2 6)
(expt-s 2 8)
(expt-s 2 7)



(defn expt [b n]
  (state-expt 1 b n))

