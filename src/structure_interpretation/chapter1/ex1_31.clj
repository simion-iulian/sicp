(ns chapter1.ex1_31)
; Product
(defn square [x] (* x x))
(defn product-linear [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product-linear term (next a) next b))))

(defn product-recur [term a next b]
  (loop [a a
         result 1.0M]
    (if (> a b)
      result
      (recur (next a) (* result (term a))))))

;Use product to compute Pi with the formula
; pi/4 = (2*4*4*6*6*8...)/(3*3*5*5*7*7*...)
(defn pi-formula [n]
  (with-precision 15 
    (* n (/ (reduce * (map #(square (* 2N %)) (range 1 n)))
            (reduce * (map #(square (+ 1 (* 2N %))) (range 1 n)))
            0.25))))
(defn pi-recur [n]
  (with-precision 15
    (* n (/ (product-recur #(square (* 2N %)) 1 inc n)
            (product-recur #(square (+ 1 (* 2N %))) 1 inc n)
            0.25))))

(Math/PI)

(/ (Math/PI) (pi-formula 10000N))
(/ (Math/PI) (pi-recur 10000N))
(#(- (pi-recur %)(pi-formula %)) 1000N)