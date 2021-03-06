(ns chapter1.counting-change)

(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1)  1 
        (= kinds-of-coins 2)  5  
        (= kinds-of-coins 3)  10  
        (= kinds-of-coins 4)  25  
        (= kinds-of-coins 5)  50))  

(defn cc [amount kinds-of-coins]
  (cond (zero? amount) 
        1
        
        (or (< amount 0) (= kinds-of-coins 0)) 
        0
        
        :else 
        (+ (cc amount (- kinds-of-coins 1))
           (cc (- amount (first-denomination kinds-of-coins))
               kinds-of-coins))))

(defn count-change [amount]
  (cc amount 5)); can use partial here

(count-change 100)