(ns chapter1.ex1_20)
(defn ackermannn [x y]
  (println (str "x:" x " y:" y))
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (ackermannn (- x 1)
                          (ackermannn x (- y 1)))))

(ackermannn 1 10)
(ackermannn 2 4)
(ackermannn 3 3)