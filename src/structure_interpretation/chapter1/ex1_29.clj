(ns chapter1.ex1_29)

;Simpson's rule
; h = (b-a)/n
; Yk = f(a + kh)


(defn cube [n] (* n n n))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn s-integral [f a b n]
  (simpson f a b n 0))

(defn integral [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (sum f
            (+ a (/ dx 2.0))
            add-dx
            b)
       dx)))

(defn simpson-term [f a h idx]
  (if (< idx 2)
    (+ (f (+ a h)) (f (+ a (* (+ idx 1) h))))
    (+ (f (+ a h)) (* 2 (f (+ a (* (+ idx 1) h)))))))

(defn simpson [f a b n idx]
  (let [h (/ (- b a ) (* 3.0 n))]
    (letfn [(s-term [x] (simpson-term f x h idx))
            (s-next [y] (simpson-term f y h (+ idx 2)))]
  (sum s-term a s-next b))))

(defn simp-integral [f a b n]
  (let [h (/ (- b a) n)]
    (letfn [(term [k]
              (* (f (+ a (* k h)))
                 (if (even? k) 2 4)))]
      (/ (* h
            (+ a (sum term 1 inc n)))
         3.0))))


(integral cube 0 1 0.00029)
(simp-integral cube 0 1 10)
(simp-integral cube 0 1 3000)
(simp-integral cube 0 1 300)
(simp-integral cube 0 1 100)
(s-integral cube 0 1 3)

