(ns chapter1.ex1_35)
; find golden ratio phi - x |> 1 + 1/x

(def tolerance 0.000001)

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
         next (f first-guess)]
    (if (close-enough? guess next)
      next
      (recur next (f next)))))

(defn fixed-point [f first-guess]
  (loop [guess first-guess
         next (f first-guess)]
    (if (close-enough? guess next)
      next
      (recur next (f next)))))

(defn phi [x] (+ 1 (/ 1 x)))
(fixed-point phi 1.0)
(fixed-point #(Math/cos %) 1.0)
(fixed-point #(Math/sin %) 1.0)


