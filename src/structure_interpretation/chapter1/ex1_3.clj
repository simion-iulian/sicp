(ns chapter1.ex1_3)

(defn biggest-of-two [x y]
  (if (> x y)
    x
    y))
(defn biggest-two-of-three [x y z] 
  (subvec (vec (sort [x y z])) 1))

(defn square-vector [x]
  (map (fn [n] (* n n)) x))

(defn square-biggest-two [x y z]
  (reduce + (square-vector (biggest-two-of-three x y z))))

