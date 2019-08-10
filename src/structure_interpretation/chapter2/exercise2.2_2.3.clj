(ns chapter2.exercise2.2-2.3
  (:require [clojure.set :as s]))

;Exercise 2.2
; Represent line segments in a plane
;Constructors
(defn point [x y]
  {:x x
   :y y})

(defn make-segment [a b]
  (if (not= a b)
    {:start a
     :end   b}
    (prn "Not a segment - points are the same")))

(defn midpoint [line]
  (let [x1 (-> line :start :x)
        x2 (-> line :end   :x)
        y1 (-> line :start :y)
        y2 (-> line :end   :y)
        half-x (/ (+ x1 x2) 2)
        half-y (/ (+ y1 y2) 2)]
    (point half-x half-y)))

(defn print-point [point]
  (prn (str "("(:x point)","(:y point)")")))

(print-point 
 (midpoint 
  (make-segment (point 1 1)
                (point 3 3))))

;Exercise 2.3
;Implement a representation for rectangles in a plane
;Constructors and selectors for rectangles
;-- Decided to use maps to contain the segments and keywords as selectors instead of dedicated functions
;Procedures for area given a rectangle, and for the peremiter
;Now implement a different representation for rectangles
;Can you design your system with suitable abstraction barriers?
;So that the same perimeter and procedures will work using either representation?

(defn square [x] (* x x))
(defn abs [n] (max n (- n)))
(def tolerance 0.00000001)
(defn close-enough? [v1 v2] (< (Math/abs (- v1 v2)) tolerance))

(defn segment-length [segment]
  (Math/sqrt 
    (+ (square (- (:x (:start segment)) 
                  (:x (:end segment))))
       (square (- (:y (:start segment)) 
                  (:y (:end segment)))))))

(segment-length (make-segment (point 1 1) 
                              (point 2 2)))

(defn common-point [first-side second-side]
  (s/intersection (-> first-side 
                      ((juxt :start :end))
                      set) 
                  (-> second-side
                      ((juxt :start :end))
                      set)))

(defn connect-sides [s1 s2]
  (->> [s1 s2]
       (map (juxt :start :end))
       flatten
       (into #{})))

(defn sides-connected? [first-side second-side]
  (= 3 (count (connect-sides first-side second-side))))

(defn hypothenuse [first-side second-side]
  (apply make-segment
         (s/difference (connect-sides first-side second-side)
                       (common-point first-side second-side))))

(defn right-angle? [first-side second-side]
  "Used to check the rectangle's construction correctness"
  (if (sides-connected? first-side second-side)
    (let [first-side-squared  (-> first-side 
                                  segment-length 
                                  square)
          second-side-squared (-> second-side 
                                  segment-length 
                                  square)
          sum-squared-sides   (+ first-side-squared second-side-squared)     
          hypothenuse-squared (-> (hypothenuse first-side second-side) 
                                  segment-length 
                                  square)]
      (close-enough? sum-squared-sides hypothenuse-squared))
    (prn "First side not connected to the second side")))

;Point impelementation
;How do I check it is enclosing?

;How do I ensure the points are bounded correctly?
;Euclidean implementation with Pythagora's theorem
;I use pythagora's theorem to prove there are right angles, as well as I check that all the sides are connected, while there being only right angles
(defn rectangle-four-sides 
  "Constructs a rectangle given 4 points, in order"
  [point1 point2 point3 point4]
  (let [side1 (make-segment point1 point2)
        side2 (make-segment point2 point3)        
        side3 (make-segment point3 point4)
        side4 (make-segment point4 point1)]
    (cond
      (not (right-angle? side1 side2)) "Points 1 2 and 3 don't define a right angle"
      (not (right-angle? side2 side3)) "Points 2 3 and 4 don't define a right angle"
      (not (right-angle? side3 side4)) "Points 3 4 and 1 don't define a right angle"
      (not (right-angle? side4 side1)) "points 4 1 and 2 don't define a right angle"
      :else (list side1 
                  side2 
                  side3
                  side4))))

(defn adjacent-sides-lengths
  [rect]
  [(segment-length (first rect))
   (segment-length (second rect))])


(def r 
  (rectangle-four-sides 
   (point 1 1)
   (point 1 3)
   (point 3 3)
   (point 3 1)))

(def r2
  (rectangle-four-sides
   (point 2.5 1)
   (point 4.5 3)
   (point 3.5 4)
   (point 1.5 2)))

(defn rect-perimeter [rect]
  (* 2 (reduce + (adjacent-sides-lengths rect))))

(adjacent-sides-lengths r)
(adjacent-sides-lengths r2)
(rect-perimeter r)
(rect-perimeter r2)

(defn rect-area [rect]
  (reduce * (adjacent-sides-lengths rect)))

(rect-area r)
(rect-area r2)
