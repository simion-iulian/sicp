(ns chapter2.section2-2-2)

;Hierarchical Structures

(def four-leaves (cons '(1 2) '(3 4 (5 6 7))))

(defn count-leaves [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :else 
        (+ (count-leaves (first x))
           (count-leaves (rest x)))))

(defn count-leaves-map [coll]
  (if (seq? coll)
    (apply + (map count-leaves-map coll))
    1))

(count-leaves four-leaves)
(count-leaves-map four-leaves)

;Exercise 2.25
;Extract the 7 from
(-> '(1 3 (5 7) 9)
    rest
    rest
    first
    rest)
(-> '(1 3 (5 7) 9)
    butlast
    last
    last)
(ffirst '((7)))
(-> '(1 (2 (3 (4 (5 (6 7))))))
    last
    last
    last
    last
    last
    last)

;Exercise 2.27
;Modify reverse that takes a list as argument 
;and returns its elements reversed 
;and all sublists deep-reversed as well

(defn deep-reverse
  [coll]
  (loop [[elem & remaining :as all] (seq coll)
         reversed                  (empty coll)]
    (if all
      (let [next-elem (if (coll? elem)
                        (deep-reverse elem)
                        elem)]
        (recur remaining (cons next-elem reversed)))
      reversed)))

(deep-reverse [1 2 3 4])
(deep-reverse [1 2 [3 4]])
(deep-reverse [1 2 [3 4 [8 7]]])
(deep-reverse [1 [2 [9 1] 5] [3 4 [8 7]]])
(deep-reverse '(1 (2 (9 1) 5) (3 4 (8 7))))

; Exercise 2.28
; Fringe - flatten a tree with leaves arrange left to right


; Now I'm saying reverse when it is a collection
; I need to say flatten when it is a collection and add it to the
; accumulated version. 
; When it is an element add it, 
; When it's a collection, flatten it and add it
(defn fringe
  [coll]
  (loop [[elem & remaining :as all] (seq coll)
         flattened                  (empty coll)]
    (if all
      (let [next-elem (if (coll? elem)
                        (fringe elem)
                        [elem])]
        (recur remaining (concat flattened next-elem)))
      flattened)))

(fringe [2 1 [4 3 [8 7]]])
(fringe '(1 (2 (9 1) 5) (3 4 (8 7))))

; Exercise 2.29 is not here but in it's own namespace
;; Mapping over trees

(defn scale-tree
  [coll factor]
  (loop [[elem & remaining :as all] (seq coll)
         scaled                  (empty coll)]
    (if all
      (let [next-elem (if (coll? elem)
                        (scale-tree elem factor)
                        (* elem factor))]
        (recur remaining (concat scaled [next-elem])))
      scaled)))

;; Exercise 2.30
(defn square-tree
  [coll]
  (loop [[elem & remaining :as all] (seq coll)
         squared                  (empty coll)]
    (if all
      (let [next-elem (if (coll? elem)
                        (square-tree elem)
                        (* elem elem))]
        (recur remaining (concat squared [next-elem])))
      squared)))

(scale-tree [1 2] 3)
(scale-tree [1 [2 7]] 3)
(square-tree [1 [2 7]])
(scale-tree [[1 [5 9]] [2 7]] 3)
(square-tree [[1 [5 9]] [2 7]])
(square-tree '(1 (5 9) (2 7)))

;;Ex 2.30
(defn square-list [items]
  (reduce (fn [acc elem] 
            (concat acc 
                    [(if (coll? elem)
                       (square-list elem)
                       (* elem elem))]))  
          []
          items))

;; Ex 2.31
(defn tree-map [items f]
  (reduce (fn [acc elem]
            (concat acc
                    [(if (coll? elem)
                       (tree-map elem f)
                       (f elem))]))
          []
          items))

(tree-map '(1 (5 9) (2 7)) #(* % %))

;; Ex 2.32
(defn subsets [s]
  (if (nil? s) '(())
    (let [remaining (subsets (next s))]
      (concat remaining 
              (map (fn [elem] 
                     (cons (first s) elem)) 
                   remaining)))))
;;Why is it working?
;; It works because it builds the subsets bottom up by mapping the first element over the empty list, then joins 
;; the remaining ones then adding at each step the subsets previously built, leading to all the possible subsets.
(subsets '(1 2 3) 0)
(subsets '(1 2 3 4) 0)
(subsets [1 2 3 4] 0)