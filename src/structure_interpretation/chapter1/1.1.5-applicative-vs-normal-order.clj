(defn p [] (p))

(defn test [x y]
  (if (= x 0)
    0
    y))

(if (= 0 (test 0 p))
  "Normal order")