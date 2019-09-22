(ns chapter1.ex1_30)

; This solution is reimplmenting the sum using loop-recur instead of linear recursion
;(define (sum term a next b) 
;  (define (iter a result) 
;    (if <??> 
;        <??> 
;        (iter <??> <??>))) 
;    (iter <??> <??>))

(defn cube [n] (* n n n))
(cube 3)

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn sum-recur [term a next b]
  (loop [a a 
         result 0M]
    (if (> a b)
      result
      (recur (next a) (+ result (term a))))))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn sum-recur-cubes [a b]
  (sum-recur cube a inc b))


(sum-cubes 1 4)
(sum-recur-cubes 1 1000000M)