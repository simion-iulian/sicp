(ns chapter1.ex1_35)
; find x^x=1000 using x |> log(1000)/log(x)

(def tolerance 0.000001)
(defn average 
  [a b] 
  (/ (+ a b) 2))

(defn log [x] (Math/log x))

(log 10)
(log Math/E)
(defn close-enough? [v1 v2](< (Math/abs (- v1 v2)) tolerance))

(Math/abs -1)
(defn fixed-point [f first-guess]
  (let [try (fn [guess] 
          (let [next (f guess)]
            (if (close-enough? guess next)
              next
              (try next))))]
  (try first-guess)))

(defn fixed-point [f first-guess]
  (loop [guess first-guess
         next (f first-guess)
         steps 0]
    (prn steps)
    (if (close-enough? guess next)
      next
      (recur next (f next) (inc steps)))))

(defn sqrt [x]
  (fixed-point #(average % (/ x %)) 1.0))


(defn x-raised-to-x [x] 
  (/ (log 1000) (log x)))
(x-raised-to-x 1.5)

;42 steps
(fixed-point x-raised-to-x 1.1)

;9 steps
(fixed-point x-raised-to-x 4.55555)

;37 steps
(fixed-point x-raised-to-x Math/E)

;13 steps
(fixed-point #(average  % (x-raised-to-x %)) 1.1)

;3 steps
(fixed-point #(average  % (x-raised-to-x %)) 4.5555555)

(fixed-point phi 1.0)
(fixed-point #(Math/cos %) 1.0)
(fixed-point #(Math/sin %) 1.0)


