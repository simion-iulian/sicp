(ns chapter2-test.clj
  (:require [clojure.test :refer :all]
            [chapter2 :refer :all]))

(deftest exercise2-2
  (testing "That functions for the rectangle system in exercise 2.2 behave as expected"
    (testing "That sides connected check works regardless of how the segment is constructed"
      (is (sides-connected? (make-segment (make-point 1 1)
                                          (make-point 1 3))
                            (make-segment (make-point 1 3)
                                          (make-point 2 3))))
      (is (sides-connected? (make-segment (make-point 1 3)
                                          (make-point 1 1))
                            (make-segment (make-point 1 3)
                                          (make-point 2 3)))))
    (testing "That we can check there is a right angle"
      (is (right-angle? (make-segment
                         (make-point 1 1)
                         (make-point 1 3))
                        (make-segment
                         (make-point 1 3)
                         (make-point 2 3)))))))