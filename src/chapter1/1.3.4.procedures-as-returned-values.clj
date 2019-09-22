(ns chapter1.procedures-as-returned-values)

;Passing procedures as arguments allows us to be more expressive
;within the body of the procedure it is passed into. It's possible
;to pass a generic procedure or abstraction that can be used
;to process the data or to create more powerful abstractions

;Even more expressive power can be achieved by returning procedures
;This is an abstraction returning another abstraction that can be passed around
;When does it make sense to use this kind of abstraction?

(defn- square [x]
  (* x x))

(def tolerance 0.00000001)

(defn close-enough? [v1 v2] (< (Math/abs (- v1 v2)) tolerance))

(defn fixed-point [f first-guess]
  (loop [guess first-guess
         next (f first-guess)]
    (if (close-enough? guess next)
      next
      (recur next (f next)))))

(defn- average [x y]
  (/ (+ x y) 2))

; Function for average between x and f(x)
(defn average-damp [f]
  (fn [x] (average x (f x))))

((average-damp square) 10)

; |> is maps to
; Sqrt is fixed point for y |> x/y
(defn sqrt [x]
   (fixed-point (average-damp (fn [y] (/ x y))) 
                1.0))
 
(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (square y)))) 
               1.0))

(sqrt 9)
(cube-root 27)

; Newton's Method
; x |> g(x) where g(x) differentiable
; A solution for g(x) = 0 is a fixed point of x |> f(x)
; f(x) = x - g(x)/Dg(x)
; Dg(x) is a derivate of g evaluated at x
; Dg(x) = (g(x + dx) - g(x))/dx
; TODO - recap what a derivative is

(def dx 0.00001)

(defn deriv 
  [g]
  (fn [x] (/ (- (g (+ x dx)) 
                (g x))
             dx)))

(defn cube 
  [x] 
  (* x x x))

((deriv cube) 5)
;; => 75.00014999664018

(defn newton-transform 
  [g]
  (fn [x]
    (- x (/ (g x) 
            ((deriv g) x)))))

(defn newtons-method 
  [g guess]
  (fixed-point (newton-transform g) 
               guess))

; y |> y^2 - x
(defn sqrt2 
  [x]
  (newtons-method (fn [y] (- (square y) x))
                  1.0))

(time (sqrt2 1000))
(time (sqrt 1000))

; Abstractions and first-class procedures
(defn fixed-point-of-transform 
  [g transform guess]
  (fixed-point (transform g) guess))

(defn sqrt3
  [x]
  (fixed-point-of-transform (fn [y] (/ x y))
                            average-damp
                            1.0))
(defn sqrt4
  [x]
  (fixed-point-of-transform (fn [y] ( - (square y) x))
                            newton-transform
                            1.0))

(sqrt3 100)
(sqrt4 100)