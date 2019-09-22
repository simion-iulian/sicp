(ns chapter1.ex1_39)

;Lamberts tangent formula
;tan x = x/ (1 - (x^2 / (3-(x^2/5 -...)))
(defn cont-frac [n d k]
  (loop [idx k
         acc 0]
    (prn "accumulate: " acc " at " idx)
    (if (< idx 1) acc
        (recur (dec idx)
               (/ (n idx)
                  (+ (d idx) acc))))))
(defn tan 
  [x]
  (/ 1.0 
     (+ 1
        (cont-frac
          (fn [k] (if (= k 0) 
                    x
                    (- (* x x))))
          (fn [k] (+ (* 2 k) 1))
          5))))

(Math/tan 1.0)
(tan 1)

(/ 1 
   (+ 1 (/ -1 
           (+ 3))))
