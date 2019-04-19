(ns chapter1.ex1_11)

(defn f3-recursive [n]
  (if (< n 3)
    n
    (+      (f3-recursive (- n 1))
       (* 2 (f3-recursive (- n 2)))
       (* 3 (f3-recursive (- n 3))))))

(defn f3-recur [n]
  (if (< n 3 ) 
    n
    (loop [f3 [0 1 2]]
      (if (> (count f3) n) 
        (f3 n) 
        (recur (conj f3
                     (+ (last f3)
                        (* 2 (second (reverse f3)))
                        (* 3 (nth (reverse f3) 2)))))))))
(defn f3-iter [f index]
      (if (zero? index)
        (last f)
        (f3-iter
         [(+ (f 0)
             (* 2 (f 1))
             (* 3 (f 2)))
          (* 2 (f 0))
          (* 3 (f 1))]
         (dec index))))

[(* 1 5) (+ (* 3 6))]

(f3-iter [2 1 0] 2)
(defn f3-iterative [n]
  (if (< n 3)
    n
    (f3-iter [[2 1 0] n])))
        
(f3-recursive 5)
(f3-recursive 4)
(f3-recursive 3)
(f3-recur 5)
(f3-recur 4)
(f3-recur 3)
(f3-iterative 3)
(f3-iterative 4)
(f3-iterative 5)
(f3-iterative 6)