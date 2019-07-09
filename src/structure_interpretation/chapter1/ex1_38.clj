(ns chapter1.ex1_38)

(def phi-value 1.608339)

(defn cont-frac [n d k]
  (loop [k k
         acc 0]
    (if (< k 1) acc
        (recur (dec k) (/ (n k) (+ (d k) acc))))))

(cont-frac
 (constantly 1.0)
 (constantly 1.0)
 16)
(defn euler-denom [i]
  (let [x (dec i)]
    (if (or (= 0 (mod x 3)))
      (* 2 (inc (/ x 3)))
      1.0)))

(euler-denom 0)
;; => 1.0
(euler-denom 1)
;; => 2
(euler-denom 2)
;; => 1.0
(euler-denom 3)
;; => 1.0
(euler-denom 4)
;; => 4
(- Math/E 2)
;; => 0.7182818284590451
(defn d [i]
  (let [i (dec i)
        idx (mod i 3)]
    (cond 
      (= idx 0) 1
      (= idx 2) 1
      (= idx 1) (* (/ (+ i 2) 3) 2))))

(+ 2 (cont-frac
 (fn [i] 1.0)
 d
 90))
