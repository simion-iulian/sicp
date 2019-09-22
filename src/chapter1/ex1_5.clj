(ns chapter1.sqrt)
; Alyssa P: Hacker doesn't see why if needs to be provided as a special form. 
; "Why can't I just define it as an ordinary procedure in terms of cond?" she asks. 
; Alyssa's friend Eva Lu Ator Claims this can be indeed done and 
; she defines a new version of if

(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(defn new-sqrt-iter [guess x]
  (new-if (good-enough? guess x)
    guess
    (new-sqrt-iter (improve guess x)
               x)))

(defn new-sqrt [x]
  (new-sqrt-iter 1.0 x))

(new-sqrt 9) ; => StackOverflowError
; Blows the stack because new-sqrt-iter gets passed as argument. 
; After the first evaluation and it gets stuck in a new-sqrt iter call