(ns chapter2-test.clj
  (:require [clojure.test :refer :all]
            [chapter2.exercise2.2-2.3 :refer :all]))

(deftest exercise2.3
  (testing "That functions for the rectangle system in exercise 2.2 behave as expected"
    (testing "That sides connected check works regardless of how the segment is constructed"
      (is (sides-connected? (make-segment (point 1 1)
                                          (point 1 3))
                            (make-segment (point 1 3)
                                          (point 2 3))))
      (is (sides-connected? (make-segment (point 1 3)
                                          (point 1 1))
                            (make-segment (point 1 3)
                                          (point 2 3)))))
    (testing "That we can check there is a right angle"
      (is (right-angle? (make-segment
                         (point 1 1)
                         (point 1 3))
                        (make-segment
                         (point 1 3)
                         (point 2 3)))))))