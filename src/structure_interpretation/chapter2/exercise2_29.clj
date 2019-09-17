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

(def big-mobile
  (make-mobile
   (make-branch 1 10)
   (make-branch 1
                (make-mobile (make-branch 3 10)
                             (make-branch 3 10)))))

(def mobile1 (make-mobile
              (make-branch 3 11)
              (make-branch 3 12)))

;b. Using selectors define a procedure total-weight
;   that returns the total weight of a mobile
; What is the temrinating condition that I need?
(defn total-weight
  [mobile]
  (reduce (fn [acc elem]
            (+ acc 
               (cond 
                 (every? number? elem) (branch-structure elem)
                 (seq? elem) (total-weight (branch-structure elem))
                 :else  elem)))
          0 
          mobile))

(total-weight mobile1)
(total-weight big-mobile)

(every? number? (left-branch mobile1))