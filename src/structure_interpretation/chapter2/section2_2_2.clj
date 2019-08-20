(ns chapter2.section2-2-2)

;Hierarchical Structures

(def four-leaves (cons '(1 2) '(3 4 (5 6 7))))

(defn count-leaves [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :else 
        (+ (count-leaves (first x))
           (count-leaves (rest x)))))

(count-leaves four-leaves)
