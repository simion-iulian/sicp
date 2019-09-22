(ns chapter2-property-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [chapter2.exercise2-2-and-2-3 :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(spec/def ::non-infinite-num
  (spec/and number? #(< -10000.0 % 10000.0)))

(spec/def ::x ::non-infinite-num)
(spec/def ::y ::non-infinite-num)

(spec/valid? ::x 1.028383)
(spec/valid? ::x 1)
(spec/valid? ::x "1.20202")

(spec/def ::point (spec/keys :req-un [::x ::y]))

(spec/valid? ::point {})
(spec/valid? ::point {:x 1.00 :y "abc"})
(spec/valid? ::point {:x 1.00 :y 1.00})
(spec/valid? ::point {:x 1.00 :y 1})

(spec/def ::start ::point)
(spec/def ::end ::point)

(def not-equal? #(not= (:start %)
                       (:end %)))

(spec/def ::segment
  (spec/and (spec/keys :req-un [::start ::end])
         not-equal?))

(gen/sample
 (spec/gen ::segment))

(defspec segment-always-positive
  50
  (prop/for-all [segment (s/gen ::segment)]
                (let [length (segment-length segment)]
                  (is (pos? length)))))