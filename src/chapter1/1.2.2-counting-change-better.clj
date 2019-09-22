(ns chapter1.counting-change-better)

(def denominations {1 1 
                    2 5 
                    3 10
                    4 25
                    5 50})

(defn cc [amount kinds-of-coins]
  (cond (zero? amount) 
        1
        (or (< amount 0) (= kinds-of-coins 0)) 
        0
        :else 
        (+ (cc amount (- kinds-of-coins 1))
           (cc (- amount (denominations kinds-of-coins))
               kinds-of-coins))))

(defn cc-iter [a b amount kinds-of-coins]
  (cond (zero? amount) 
        1
        (or (< amount 0) (= kinds-of-coins 0)) 
        0
        :else 
        (+ (cc amount (- kinds-of-coins 1))
           (cc (- amount (denominations kinds-of-coins))
               kinds-of-coins))))

(defn count-change [amount]
  (cc amount (count denominations))); can use partial here

(count-change 100)