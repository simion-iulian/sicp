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