(ns chapter1.ex1_44)

;smoothing

;given f a function
;and dx a small number
;then smoothed version of f is
;smooth(f(x)) is average of (f(x-dx), f(x), f(x+dx)) 
;also get the n-fold smoothed function (apply smoothed repeteadly)

(defn compose
  [f g]
  (fn [x] (f (g x))))

(defn repeated [f n]
  (if (= n 1)
    (fn [x] (f x))
    (compose f (repeated f (dec n)))))

(def dx 0.000001)

(defn smooth [f]
  (fn [x]
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.00)))

(defn jumpy-function [x]
  (if (< x 0)
    0
    1))

((smooth jumpy-function) dx)
((smooth jumpy-function) 0)
((smooth jumpy-function) -1)
((smooth jumpy-function) (- dx))
((smooth jumpy-function) (- (/ dx 20)))
 
(defn n-smooth [f n]
  (fn [x]
    (((repeated smooth n) f) x)))

((n-smooth jumpy-function 10) 0)