(ns chapter1.abstractions-with-higher-order-procedures)

(defn cube [n] (* n n n ))
(cube 3)

;1.3.1 Procedures as arguments

(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a 
       (sum-integers (+ a 1) b))))
(sum-integers 1 3)

(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) 
       (sum-cubes (+ a 1) b))))

(sum-cubes 1 3)

(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
(/ (Math/PI) 8) 
(pi-sum 1 100)

(defn pattern-sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (pattern-sum term (next a) next b))))

(defn recur-sum [term a next b]
  (loop [term-a (term a)
         next-a (next a)]
    (if (> a b)
      0
      (recur (+ term-a next-a) a))))

(defn sum-pattern-cubes [a b]
  (pattern-sum cube a inc b))

(defn sum-pattern-integers [a b]
  (pattern-sum identity a inc b))
; pi sum with patterns and inner functions
(defn pi-pattern-sum [a b]
  (letfn [
          (pi-term [x] (/ 1.0 (* x (+ x 2))))
          (pi-next [y] (+ y 4))]
  (pattern-sum pi-term a pi-next b)))

(identity 5)
(sum-cubes 1 3)
(sum-pattern-integers 1 4)

(defn sum-recur-cubes [a b]
  (recur-sum cube a inc b))

(sum-pattern-cubes 1 4)
(sum-recur-cubes 1 4)
(Math/PI)
(* 8 (pi-pattern-sum 1 10000))

;INTEGRAL for a function
(defn integral [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (pattern-sum f
                    (+ a (/ dx 2.0))
                    add-dx
                    b)
       dx)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.00029)