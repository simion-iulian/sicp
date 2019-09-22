(ns chapter2.section2-2-1)

;Representing sequences
;Exercise 2.17. Implement last

;This works only on lists and arrays
(defn last [coll]
  (nth coll (- (count coll) 1)))

;This works with maps and sets too, though there is no guaranteed order for maps and sets
(defn last [coll]
  (if (= 1 (count coll))
    (first coll)
    (recur (rest coll))))

(last [1 2 3])

;Exercise 2.18
;Define a procedure reverse that returns the same list in reverse order

(defn reverse 
  [coll]
  (loop [[element & remaining-elements :as all] (seq coll)
         reversed                               (empty coll)]
    (if all
      (recur remaining-elements (cons element reversed))
      reversed)))

(reverse '(1 2 3))
(reverse [1 2 3])

;Clojure demo to show implicit loop binding
;Here citruses progressively loses one element until nil just by calling recur on the citruses binding
;Citruses gets the first item from the input and the items following the ampersand get the rest
(def citrus-list ["lemon" "orange" "grapefruit"])
(defn display-citrus [input-citruses]
  (loop [[citrus & citruses] input-citruses]
    (println citrus " and " citruses)
    (if citrus (recur citruses))))

(display-citrus citrus-list)

;Exercise 2.19
;Due to this implementation taking advantage of the Clojure's vector direct access

(def us-coins [1 5 10 25 50])
(def uk-coins [0.5 1 2 5 10 20 50 100])

(defn cc 
  [amount coin-values]
    (cond (zero? amount) 
          1
          (or (neg? amount) (empty? coin-values)) 
          0
          :else 
          (+ (cc amount (rest coin-values))
             (cc (- amount 
                    (first coin-values))
                 coin-values))))

(defn count-change 
  ([amount currency-denominations] 
   (cc amount 
       currency-denominations)))

(count-change 100 us-coins)
;Reverse is faster than the increasing order
;Even the shuffled order is faster than the increasing order
(with-out-str
  (time 
   (count-change 100 (reverse uk-coins))))
;; => "Elapsed time: 657.073165 msecs"

(with-out-str
  (time 
   (count-change 100 uk-coins)))
;; => "Elapsed time: 2634.309421 msecs"

(with-out-str
  (time
   (count-change 100 (shuffle uk-coins))))
;; => "Elapsed time: 1696.289458 msecs"


;Exercise 2.20
; Filter by parity of first arg

(defn same-parity [& args]
  (let [by (if (-> args first even?) 
             even? 
             odd?)]
    (filter by args)))

(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5 6)

;Mapping over lists
;Exercise 2.21

(defn square-list [items]
  (if (empty? items)
    nil
    (cons (* (first items)
             (first items))
          (square-list (rest items)))))

(square-list '(2 3 4))

(defn square-list [items]
  (map #(* % %) items))

(square-list '(2 3 4))


;Exercise 2.22

(defn square-list [items]
  (loop [things (seq items)
         answer []]
    (if (empty? things)
      answer
      (recur (rest things)
             (cons
              (#(* % %) (first things))
              answer)))))
;when the answer is first I start building the result by squaring that and it always ends up at the end
;When the answer is second the result is always first, but the first element in the passed down result ends up being the last element
;So it doesn't matter the order, the result is the same
(square-list [1 2 3])

;;