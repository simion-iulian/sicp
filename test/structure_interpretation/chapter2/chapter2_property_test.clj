(ns chapter2-property-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            ; [chapter2.exercise2.2-2.3 :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(s/def ::non-infinite-num
  (s/and number? #(< % 1000.0)))

(s/def ::x ::non-infinite-num)
(s/def ::y ::non-infinite-num)

(s/valid? ::x 1.028383)
(s/valid? ::x 1)
(s/valid? ::x "1.20202")

(s/def ::point (s/keys :req-un [::x ::y]))

(s/valid? ::point {})
(s/valid? ::point {:x 1.00 :y "abc"})
(s/valid? ::point {:x 1.00 :y 1.00})
(s/valid? ::point {:x 1.00 :y 1})

(s/def ::start ::point)
(s/def ::end ::point)

(def not-equal? #(not= (:start %) 
                       (:end %)))

(s/def ::segment 
  (s/and (s/keys :req-un [::start ::end])
         not-equal?))
(gen/sample 
 (s/gen ::segment)
 100)

(defspec segment-always-positive
  50
  (prop/for-all [segment (s/gen ::segment)]
                (let [length (segment-length segment)]
                  (is (pos? length)))))

