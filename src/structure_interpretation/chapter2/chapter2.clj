(ns chapter2)

(defn mul [& args]
  (apply * args))

(defn linear-combination [a b x y]
  (+ (mul a x)
     (mul b y)))

(linear-combination 1 1 1 1)

;Example Airthmetic Operations for Rational Numbers

(defn make-rat [n d]
  (cond 
    (or (and (< d 0) (> n 0))) (list (- n) (- d))
    (< n 0) (list n d)
    :else (list n d)))

(make-rat -1 -2)
(make-rat -1 2)
(make-rat 1 -2)
(make-rat 1 2)

(def x 10)
(- 10)

(list (- x))
(defn numer [x]
  (first x))
(defn denom [x]
  (last x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(defn print-rat [x]
  (prn (/ (numer x)
          (denom x))))

(print-rat (make-rat 1 2))
(/ (numer (make-rat 1 2))
   (denom (make-rat 1 2)))

;Exercise 2.2
; Represent line segments in a plane
;Constructors
(defn make-point [x y]
  (list x y))
(make-point 1 2)

(defn make-segment [point1 point2]
  (list point1 point2))
;Selectors
(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (last segment))

(defn x-point [point]
  (first point))

(defn y-point [point]
  (last point))

(defn midpoint [line]
  (let [x1 (x-point (start-segment line))
        x2 (x-point (end-segment line))
        y1 (y-point (start-segment line))
        y2 (y-point (end-segment line))]
    (make-point (/ (+ x1 x2) 2)
                (/ (+ y1 y2) 2))))

(defn print-point [point]
  (prn (str "("(x-point point)","(y-point point)")")))

(print-point 
 (midpoint 
  (make-segment (make-point 1 1)
                (make-point 3 3))))

;Exercise 2.3
;Implement a representation for rectangles in a plane
;Constructors and selectors for rectangles
;Procedures for area given a rectangle, and for the peremiter
;Now implement a different representation for rectangles
;Can you design your system with suitable abstraction barriers?
;So that the same perimeter and procedures will work using either representation?
(defn square [x] (* x x))
(defn abs [n] (max n (- n)))
(def tolerance 0.00000001)
(defn close-enough? [v1 v2] (< (Math/abs (- v1 v2)) tolerance))

(defn segment-length [s]
  (Math/sqrt 
    (+ (square (- (x-point (start-segment s)) (x-point (end-segment s))))
       (square (- (y-point (start-segment s)) (y-point (end-segment s)))))))

(segment-length (make-segment (make-point 1 1) 
                              (make-point 2 2)))
;Do I make use only of points or of segments?
(defn right-angle? [first-side second-side]
  "Used to check the rectangle's construction correctness"
  (if (= (end-segment first-side)
         (start-segment second-side))
    (let [first-side-squared  (square (segment-length first-side))
          second-side-squared (square (segment-length second-side))
          pythagoras-square   (+ first-side-squared second-side-squared)
          hypothenuse-squared (square (segment-length (make-segment 
                                                       (start-segment first-side) 
                                                       (end-segment second-side))))]
      (close-enough? pythagoras-square hypothenuse-squared))
    (prn "First side not connected to the second side")))

(right-angle? (make-segment
               (make-point 1 1)
               (make-point 1 3))
              (make-segment
               (make-point 1 3)
               (make-point 2 3)))
;Point impelementation
;How do I check it is enclosing?
(defn make-rect 
  "Constructs a rectangle given 4 points, in order"
  [point1 point2 point3 point4]
  (let [side1 (make-segment point1 point2)
        side2 (make-segment point2 point3)        
        side3 (make-segment point3 point4)
        side4 (make-segment point4 point1)]
    (cond
      (right-angle? side1 side2) (prn "Points 1 2 and 3 don't define a right angle")
      (right-angle? side2 side3) (prn "Points 2 3 and 4 don't define a right angle")
      (right-angle? side3 side4) (prn "Points 3 4 and 1 don't define a right angle")
      (right-angle? side4 side1) (prn "points 4 1 and 2 don't define a right angle")
      :else (list side1 
                  side2 
                  side3
                  side4))))

(defn rect-point [rect x]
  (if (<= 0 x 3)
    (nth rect x)
    (prn "Outside allowed boundaries")))

;This works only for lines parallel to the axes, how can it work for all lines?
(defn adjacent-sides [rect]
  (throw Exception. "Get adjacent sides"))

(defn rect-perimeter [rect]
  (let [twice-one-side   (* 2 (abs 
                               (- (-> rect (rect-point 0) x-point)
                                  (-> rect (rect-point 1) x-point))))
        twice-other-side (* 2 (abs
                               (- (-> rect (rect-point 1) x-point)
                                  (-> rect (rect-point 2) x-point))))]
    (+ twice-one-side twice-other-side)))

(defn rect-area [rect]
  (let [one-side (abs (- (-> rect (rect-point 0) x-point)
                         (-> rect (rect-point 1) x-point)))
        adjacent-side (abs (- (-> rect (rect-point 1) x-point)
                              (-> rect (rect-point 2) x-point)))]
    (* one-side adjacent-side)))

;How do I ensure the points are bounded correctly?
;This implementation is tightly bound to order of things.
;Could the constructor `construct` the rectangle 
;instead of just holding the data in a structure?
(rect-perimeter 
 (make-rect 
  (make-point 1 1)
  (make-point 1 3)
  (make-point 3 3)
  (make-point 3 1)))
