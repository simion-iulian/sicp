(ns chapter2.section2-1-3)

(defn cons [x y]
  (fn [m]
    (cond 
      (= m 0) x
      (= m 1) y
      :else (str "Argument not 0 or 1: CONS" m))))

(def cons1 (cons 1 2))

(defn car [z] (z 0))
(defn cdr [z] (z 1))

(car cons1)
(cdr cons1)

;also see exercise 2.4