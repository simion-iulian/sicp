(ns chapter1.ex1_41)

(defn double 
  [procedure]
  (fn [x] (procedure (procedure x))))

((double inc) 1)

;prediction is 2^(2^2) = 8 * inc
;therefore 5+16=13 
(((double (double double)) inc) 5)
;; => 21 ;; was actually 16, not 8, so it is 2^(k+1) calls
((double inc) 1)
(((double (double double)) inc) 5)

