(ns chapter2.section2-2-3)

;2.2.3 Sequences as Conventional Interfaces

;Ex 2.33
(defn my-map [p sequence]
  (reduce (fn [acc elem]
            (conj acc (p elem)))
          (empty sequence)
          sequence))

(my-map (partial * 3) [1 2 3 4])

;Exercise 2.34
; Horner's rule
; a[n]*x^n + a[n-1]*x^(n-1)+...+a[1]*x+a[0]
; ((a[n]*x + a[n-1])*x + ... + a[1])*x + a[0]

(defn pow 
  [base to]
  (reduce *' (repeat to base)))

(pow 100 30)

(defn polynom-eval 
  [x coefficient-sequence]
  (->> coefficient-sequence
       (reduce (fn [acc this-coeff]
                 (let [power (count acc)]
                   (conj acc (* (pow x power) 
                                this-coeff))))
               [])
       (apply +)))

(defn horner-eval
  [x coefficient-sequence]
  (->> (reverse coefficient-sequence)
       (reduce (fn [acc coeff]
                 (+ coeff (* x acc))))))

(polynom-eval 2 [1 3 0 5 0 1])
(horner-eval 2 [1 3 0 5 0 1])
;=> should be 1+3*2+5*2^3+2^5
;=> total: 1 + 6 + 24 + 32 = 79

;Ex 2.35
(defn count-leaves 
  [coll]
  (if (seq? coll)
    (reduce + (map count-leaves coll))
    1))

(def four-leaves (cons '(1 2) '(3 4 (5 6 7))))

(count-leaves four-leaves)

;Ex 2.36

(defn accumulate-n
  "Like reduce, but takes in a sequence of sequences of the same length, 
   and applies the operation on elements vertically instead of on each sequence"
  [op init seqs]
  {:pre [(apply = (map count seqs))]}
  (->> seqs
       (apply interleave)
       (partition (count seqs))
       
       (map (partial reduce op init))))

(accumulate-n + 10 [[1 2 3 1] [6 6 6 1] [7 7 7 1]])
(accumulate-n + 10 [[1 2 3 1] [6 6 6 1] [7 7 1]])

(count)