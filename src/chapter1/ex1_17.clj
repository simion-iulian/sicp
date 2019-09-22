(ns chapter1.ex1_17)

(defn square [x] (* x x))

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

(defn double [a]
  (+ a a))
(defn halve [n]
  (/ n 2))

(defn mul [a b]
  (prn a ":" b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))

(defn mul-log [a b]
  (prn a ":" b)
  (cond (= b 0) 0
        (even? b) (mul (double a) (halve b))
        (odd? b) (+ a (mul a (dec b)))))

(defn mul-iter [n a b]
  (prn n "-" a ":" b )
  (cond (= b 0) n
        (even? b) (mul-iter (+ n (double a)) a (halve b))
        (odd? b) (mul-iter (+ n a) a (dec b))))

(defn mul-i [a b]
  (mul-iter 0 a b))

(* 3 4)
(mul 3 4)

