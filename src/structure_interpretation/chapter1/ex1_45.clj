(ns chapter1.ex1_45)

;Find nth root

(def tolerance 0.00000001)

(defn close-enough? [v1 v2] (< (Math/abs (- v1 v2)) tolerance))

(defn fixed-point [f first-guess]
  (loop [guess first-guess
         next (f first-guess)]
    (if (close-enough? guess next)
      next
      (recur next (f next)))))

(defn repeated-recur
  [f n]
  (loop [idx n
         layers f]
    (if (= idx 1)
      layers
      (recur (dec idx) (comp layers f)))))

(defn- average [x y]
  (/ (+ x y) 2))

; Function for average between x and f(x)
(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn compose
  [f g]
  (fn [x] (f (g x))))

;1. use recursion
; x|>x+1 f(f...(f(x))) |> x+n
; squaring would be 2^nth power of x
(defn repeated [f n]
  (if (= n 1)
    (fn [x] (f x))
    (compose f (repeated f (dec n)))))

(defn nth-power [x n] (reduce * (repeat n x)))

(defn floored-binary-log [x] 
  (int
   (/ (Math/log x)
      (Math/log 2))))

(defn nth-root [x n]
  (fixed-point 
    ((repeated
      average-damp
      (floored-binary-log n)) 
     (fn [y]
       (/ x (nth-power y (dec n)))))
    1.0))

(defn nth-root-recur [x n]
  (fixed-point
   ((repeated-recur
     average-damp
     (floored-binary-log n))
    (fn [y]
      (/ x (nth-power y (dec n)))))
   1.0))   

(nth-root 27 3)
(nth-root 81 4)
(nth-root 243 5)
(nth-root 729 6)
;(nth-root (reduce * (repeat 6 3)) 6)
;(nth-root (reduce * (repeat 7 3)) 7)
;(nth-root (reduce * (repeat 8 3)) 8)
(nth-root (reduce * (repeat 9 3)) 9)
;(nth-root (reduce * (repeat 10 3)) 10)
(nth-root (reduce * (repeat 12 3)) 12)
(nth-root (reduce * (repeat 20 3)) 20)
(time (nth-root (reduce * (repeat 40 3N)) 40))
(time (nth-root-recur (reduce * (repeat 40 3N)) 40))
(time (nth-root (reduce * (repeat 100 3N)) 100))
(time (nth-root-recur (reduce * (repeat 100 3N)) 100))