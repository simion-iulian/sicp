(ns chapter1.ex1_12)
;Pascal's Triangle
;    1 
;   1 1
;  1 2 1 
; 1 3 3 1
;1 4 6 4 1

(defn pascal [n]
  (if (= n 1)
    '(1)
    (map + (concat [0] (pascal (dec n)))
                (concat (pascal (dec n)) [0]))))

(pascal 5)
(pascal 4)
(pascal 3)
(pascal 2)
(pascal 1)
(triangle 1 2)
(triangle 1 3)
(triangle 1 4)
(triangle 1 5)