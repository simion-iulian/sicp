(ns chapter1.ex1_38)

(defn cont-frac [n d max]
  (loop [k max
         acc 0]
    (prn " last term: " (+ (d k) acc))
    (prn "accumulate: " acc)
    (if (< k 1) acc
        (recur (dec k) (/ (n k) (+ (d k) acc))))))

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
 10))
