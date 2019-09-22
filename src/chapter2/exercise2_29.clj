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
                 (every? number? elem) (branch-structure elem)
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

(defn calculate-torque 
  [branch]
  (* (branch-length branch)
     (if (coll? (branch-structure branch))
       (* (calculate-torque (left-branch  (branch-structure branch)))
          (calculate-torque (right-branch (branch-structure branch))))
       (branch-structure branch))))

(defn balanced? [mobile]
  (reduce #(and (= %1 %2) 
                (number? %2)) 
          (map calculate-torque mobile)))