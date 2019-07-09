(ns chapter1.ex1_37)
; find x^x=1000 using x |> log(1000)/log(x)

(def tolerance 0.000000001)
(defn average 
  [a b] 
  (/ (+ a b) 2))

(defn log [x] (Math/log x))

(log 10)
(log Math/E)
(defn close-enough? [v1 v2](< (Math/abs (- v1 v2)) tolerance))
(defn close-within? [v1 v2 precision](< (Math/abs (- v1 v2)) precision))

(Math/abs -1)
(defn fixed-point-recur [f first-guess]
  (let [try (fn [guess] 
          (let [next (f guess)]
            (if (close-enough? guess next)
              next
              (try next))))]
  (try first-guess)))

(defn fixed-point [f first-guess]
  (loop [guess first-guess
         next (f first-guess)
         ]
    (if (close-enough? guess next)
      next
      (recur next (f next)))))

(defn phi [x] (+ 1.0 (/ 1.0 x)))

(fixed-point phi 1.6)
(def phi-value (fixed-point phi 1.6))

(defn cont-frac
  [n d k]
  (loop [step k
         frac (/ (n step) (d step))]
    (if (<= step 1)
      frac
      (recur (dec step) (/ (n step) 
                           (+ (d step) frac))))))

(/ 1 phi-value)
;; => 0.6180339886704432
;; => 0.6180338134001252

;0.6180339889579018
(cont-frac 
 (constantly 1.0) 
 (constantly 1.0) 
 16)


