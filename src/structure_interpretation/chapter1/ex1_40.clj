(ns chapter1.ex1_40)

; (newtons-method (cubic a b c) 1)
; to get zeros for x^3 + a*x^2 + b*x + c
; x|>x ^3 + a*x ^2 + b*x + c

(def tolerance 0.00000001)

(defn close-enough? [v1 v2] (< (Math/abs (- v1 v2)) tolerance))

(defn fixed-point [f first-guess]
  (loop [guess first-guess
         next (f first-guess)]
    (if (close-enough? guess next)
      next
      (recur next (f next)))))

(def dx 0.00001)

(defn deriv
  [g]
  (fn [x] (/ (- (g (+ x dx))
                (g x))
             dx)))

(defn newton-transform
  [g]
  (fn [x]
    (- x (/ (g x)
            ((deriv g) x)))))

(defn newtons-method
  [g guess]
  (fixed-point (newton-transform g)
               guess))

(defn cubic [a b c]
  (fn [x]
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(newtons-method (cubic -3 -144 432) 1.0)
(newtons-method (cubic -7 4 12) 1.0)

(defn fixed-point-of-transform
  [g transform guess]
  (fixed-point (transform g) guess))