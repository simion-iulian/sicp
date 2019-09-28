(ns chapter2.exercise2-29)

; Binary mobile - two branches
; Left & Right

(defn make-mobile
  [left right]
  (list left right))

; Branch is constructed from a length and structure
; number or another mobile

(defn make-branch 
  [length structure]
  (list length structure))

;a. Write the corresponding selectors left-branch and right-branch

(defn left-branch
  [mobile]
  (first mobile))

(defn right-branch
  [mobile]
  (second mobile))

(defn branch-length
  [branch]
  (first branch))

(defn branch-structure
  [branch]
  (second branch))

;b. Using selectors define a procedure total-weight
;   that returns the total weight of a mobile
; What is the temrinating condition that I need?
(defn total-weight
  [mobile]
  (reduce (fn [acc elem]
            (+ acc 
               (cond 
                 (and (coll? elem) (every? number? elem)) (branch-structure elem)
                 (coll? elem) (total-weight (branch-structure elem))
                 :else  elem)))
          0 
          mobile))

;;c. A mobile is said to be balanced 
;;  if the torque applied to its top-left branch 
;;  is equal to that applied by it's top-right branch
;;  Torque is the length multiplied by the weight of the rod
;; TODO: The algorithm for now just goes recursively in to check
;;       - It needs to check if all the hanging structures are balanced as well. 

(defn torque 
  [branch]
  (* (branch-length branch)
     (if (number? (branch-structure branch))
       (branch-structure branch)
       (total-weight (branch-structure branch)))))

; How do I plan to balance this?
; Get the weight for each rod at each level. Compare all torques on a branch 
;  and make sure that whenever there is branching there is equality
;  How do I calculate the torque?
;  What does need to be checked at the same time at each step?
(defn balanced? 
  [mobile]
  (let [inner-balance? #(if (number? (branch-structure %))
                          true
                          (balanced? (branch-structure %)))
        check? #(apply = (map % mobile))]
    (->> (map check? [torque inner-balance?])
         (every? true?))))

