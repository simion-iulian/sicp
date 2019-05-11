(ns chapter1.ex1_37)


(fixed-point phi 1.6)
(def phi-value (fixed-point phi 1.6))

(defn cont-frac
  [n d k]
  (loop [step k
         next (/ (n step) (d step))]
    (prn step "-" next)
    (if (= step 0)
      next
      (recur (dec step) (/ (n step) 
                           (+ (d step) next))))))

(/ 1 phi-value)

;0.6180339889579018
;1/phi
(cont-frac 
 (constantly 1.0) 
 (constantly 1.0) 
 11)

;e constant

(cont-frac
 (constantly 1.0)
 (fn [i] 
   (let [x (dec i)]
     (if (or (= 0 (mod x 3)))
       (* 2 (inc (/ x 3)))
       1)))
 5)

(+ 2 (cont-frac
      (constantly 1.0)
      (fn [i]
        (let [x (dec i)]
          (if (or (= 0 (mod x 3)))
            (* 2 (inc (/ x 3)))
            1)))
      5))
(/ Math/E (cont-frac
           (constantly 1.0)
           (fn [i]
             (let [x (dec i)]
               (if (or (= 0 (mod x 3)))
                 (* 2 (inc (/ x 3)))
                 1.0)))
           50))

(defn test-fun 
  [i]
  (let [x (dec i)]
    (if (or (= 0 (mod x 3)))
      (* 2 (inc (/ x 3)))
      1.0)))