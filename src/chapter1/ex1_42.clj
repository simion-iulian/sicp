(ns chapter1.ex1_42)

; Composition of f and g
; x|>f(g(x))

(defn square [x] (* x x))

(defn compose
  [f g] 
  (fn [x] (f (g x))))

((compose square inc) 6)

