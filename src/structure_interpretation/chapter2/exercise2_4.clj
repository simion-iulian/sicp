(ns chapter2.exercise2-4)

(defn cons [x y]
  (fn [m]
    (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(def c1 (cons 1 2))

(car c1)
(cdr c1)

(defn carr [x y]
  (letfn [(cons [x y] (fn [message-extractor] 
                        (message-extractor x y)))
          (car  [extractor-passer] 
                (extractor-passer (fn [p q] p)))
          (cdr  [z]   (z (fn [p q] q)))]
    (car (cons x y))))