(ns chapter2.section2-2-3)

;2.2.3 Sequences as Conventional Interfaces

;Ex 2.33
(defn my-map [p sequence]
  (reduce (fn [acc elem]
            (conj acc (p elem)))
          (empty sequence)
          sequence))

(my-map (partial * 3) [1 2 3 4])

;Exercise 2.34
; Horner's rule
; a[n]*x^n + a[n-1]*x^(n-1)+...+a[1]*x+a[0]
; ((a[n]*x + a[n-1])*x + ... + a[1])*x + a[0]

(defn pow 
  [base to]
  (reduce *' (repeat to base)))

(pow 100 30)

(defn polynom-eval 
  [x coefficient-sequence]
  (->> coefficient-sequence
       (reduce (fn [acc this-coeff]
                 (let [power (count acc)]
                   (conj acc (* (pow x power) 
                                this-coeff))))
               [])
       (apply +)))

(defn horner-eval
  [x coefficient-sequence]
  (->> (reverse coefficient-sequence)
       (reduce (fn [acc coeff]
                 (+ coeff (* x acc))))))

(polynom-eval 2 [1 3 0 5 0 1])
(horner-eval 2 [1 3 0 5 0 1])
;=> should be 1+3*2+5*2^3+2^5
;=> total: 1 + 6 + 24 + 32 = 79

;Ex 2.35
(defn count-leaves 
  [coll]
  (if (seq? coll)
    (reduce + (map count-leaves coll))
    1))

(def four-leaves (cons '(1 2) '(3 4 (5 6 7))))

(count-leaves four-leaves)

;Ex 2.36

(defn- all-seqs-same-length?
  [seqs]
  (apply = (map count seqs)))

;it is accumulate-n in the book
(defn accumulate-vertically
  "Like reduce, but takes in a sequence of sequences of the same length, 
   and applies the operation on elements vertically instead of horizontally"
  [op init seqs]
  {:pre [(all-seqs-same-length? seqs)]}
  (let [column-size (count seqs)]
        ;; put all elements one after the other, basically put all columns in one array
    (->> (apply interleave seqs)
       ;; then group them by column, such that now we have sequences of columns and not of rows
         (partition column-size)
       ;; then apply the operation on each grouping to get the result
         (map (partial reduce op init)))))

(accumulate-vertically + 10 [[1 2 3 1] [6 6 6 1] [7 7 7 1]])
; (accumulate-vertically + 10 [[1 2 3 1] [6 6 6 1] [7 7 1]]) ;; assertion failing


;; Exercise 2.37
;; Matrix manipulation exercise
;; Vectors v = (v[i])
;; matrices m = (m[i][j]) - sequences of vectors
;; [[1 2 3 9]
;;  [5 3 6 4]
;;  [8 5 3 2]]

;; (dot-product v w) - sigma[i] v[i] w[i]
;; (matrix-*-vector m v)
;; (matrix-*-matrix m n)
;; (transpose m)

(defn transpose
  "Like reduce, but takes in a sequence of sequences of the same length, 
   and applies the operation on elements vertically instead of horizontally"
  [seqs]
  {:pre [(all-seqs-same-length? seqs)]}
  (let [column-size (count seqs)]
    (->> (apply interleave seqs)
         (partition column-size))))


(transpose [["00" "01" "02"]
            ["10" "11" "12"]])

(defn dot-product
  "Product of the two vectors multiplied at each index" 
  [seq1 seq2]
  (->> (transpose [seq1 seq2])
       (map (partial reduce *))
       (reduce *)))
(dot-product [1 2] [3 4])

(defn matrix-*-vector
  "t[i] |-> sigma[j] = m[i][j] * v[j]"
  [matrix multiplier-vector]
  {:pre [(all-seqs-same-length? (conj matrix multiplier-vector))]} 
  (map (partial dot-product multiplier-vector) matrix))

(matrix-*-vector [[1 2 3] [3 4 5]] [3 4 7])

(defn matrix-*-matrix
  [matrix1 matrix2]
  (->> (transpose matrix2)
       (map (partial matrix-*-vector matrix1))))

(matrix-*-matrix [[1 2 3] 
                  [4 5 6]]
                 [[2 3] 
                  [4 5] 
                  [6 7]])

(matrix-*-matrix [[2 3]
                  [4 5]
                  [6 7]]
                 [[1 2 3]
                  [4 5 6]])

;; Nested mappings
;; Find all ordered pairs of distinct positive integers i and j where 1 <= j < i <= n

(defn- prime? 
  [n]
  (.isProbablePrime (.toBigInteger (bigint n)) 10))

(def sum (partial apply +))

(defn prime-sum-pairs
  "Create all combinations of 1 <= j < i <= n"
  [n]
  (->> (range 1 (inc n))
       (mapcat (fn [i]
                 (map (fn [j] (list i j))
                      (range 1 i))))
       (filter #(prime? (sum %)))
       (map #(concat % [(sum %)]))))

(defn for-pairs-sum
  "Create all combinations of 1 <= j < i <= n
   This is using idiomatic Clojure sequence comprehensions to solve the same problem
   avoiding the need of accumulating, enumerating, mapping and filtering in SICP"
  [n]
  (for [i (range 1 (inc n))
        j (range 1 i)
        :let [sum (+ i j)]
        :when (prime? sum)]
    (list i j sum)))

(time (prime-sum-pairs 6))
  (time (for-pairs-sum 6))

(defn subsets 
  [s]
  (if (nil? s) '(())
      (let [remaining (subsets (next s))]
        (concat remaining
                (map (fn [elem]
                       (cons (first s) elem))
                     remaining)))))

(subsets [1 2 3])

;; Plan for premutations
;; Example of permutations
;; => ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
;; Observation: one can take 1 2 3 and just rotate the numbers by one
;; Then invert all the rotated sets and have all the permutations for (1 2 3)
;; SICP plan for generating permutations in a set S:
;; For each item x in S,
;;
;; **recursively** generate 
;; the sequence of permutations of S - x
;; and adjoin x to the front of each one
;;
;; This yields, for each x in S, 
;; the sequence of permutations of S that begin with x
;; Combining these sequences for all x gives all the permutations of S  

(defn permutations
  [some-set call-cnt]
  (prn (format "%s %10s" "set: " (str (seq some-set))) (format "call-no %d" call-cnt))
  (if-not (seq some-set)
    [[]]
    (mapcat (fn [x]
              ;; this map constructs the next permutation. 
              ;;It is passed where each element is removed by each call to the first mapping
              (map (fn [p]
                     (cons x p))
                   (permutations (remove (partial = x) some-set) (inc call-cnt)))) ;; this also removes duplicates
            ;;1. X is being passed into the mapcat/flatmap such that it is removed in the next call to permutations
            some-set)))

;;the map below is being passed the collection that is built from above with one less element
;; and told to join in the beginning the removed element 
;; to the result of the sub-permutations built without it

;; (permutations [1 2 3] 0)


;; Ex 2.41
(defn ordered-triples-less-than
  "Create all combinations of 1 <= k < j < i <= n
   This is using idiomatic Clojure sequence comprehensions to solve the same problem
   avoiding the need of accumulating, enumerating, mapping and filtering in SICP"
  [n s]
  (for [i (range 1 (inc n))
        j (range 1 i)
        k (range 1 j)
        :let [sum (+ i j k)]
        :when (= sum s)]
    (list i j k)))

;; 2.42 Eight Queens problem
;; How to place eight queens on the chessboard so that no queen is in check from any other
;; One solution:
;; Work across the board, placing a queen in each column.
;; Once we have placed k-1 queens, 
;; we must place the kth queen in a position where it does not check with any of the queens already on the board
;; Formulating recursively:
;; Assume we already generated the sequence of all possible ways to place k-1 queens in the first k-1 columns
;; For each of these ways, generate an extended set of positions by placing a queen in each row of the kth column
;; Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens
;; This produces the sequence of all ways to place k queens in the first k columns
;; By continuing this process, we will produce not only one solution, but all solutions to the puzzle

;; I'm using a different solution than the one proposed in the book without passing in the kth column, thus comparing all positions with each other.
;; If I pass it the algorithm can be made faster by checkig only on that specific column.
;; I found this solution to be shorter and to work in satisfactory time for this exercise.
(defn safe?
  [positions]
  (letfn [(safe-position?
            [[new-col new-row] [existing-col existing-row]]
            (and (not= new-row existing-row)
                 (not= new-col existing-col)
                 (not= (- new-row new-col) (- existing-row existing-col))
                 (not= (+ new-row new-col) (+ existing-row existing-col))))]
    (every?
     true?
     (for [pos positions]
       (->> (remove #{pos} positions)
            (every? (partial safe-position? pos)))))))

(defn queens [board-size]
  (letfn [(queen-cols
           [kth-column]
           (if (zero? kth-column)
             [#{}]
             (filter
              safe?
              (mapcat 
               (fn [rest-of-queens]
                 (->> (for [new-row (range 1 (inc board-size))] 
                        [kth-column new-row])
                      (map (partial conj rest-of-queens))))
               (queen-cols (dec kth-column))))))]
   (queen-cols board-size)))

;; (time (count (queens 4)))
;; (time (count (queens 5)))
;; (time (count (queens 6)))
;; (time (count (queens 7)))
;; (time (count (queens 8)))
;; (time (count (queens 9)))
;; (time (count (queens 10)))



