(ns chapter2.exercise2-29-test
  (:require [clojure.test :refer :all]
            [chapter2.exercise2-29 :refer :all]))

(def balanced-mobile2 (make-mobile
                       (make-branch 3 (make-mobile
                                       (make-branch 3 23)
                                       (make-branch 3 23)))
                       (make-branch 3 (make-mobile
                                       (make-branch 3 23)
                                       (make-branch 3 23)))))

#_(def unbalanced-mobile2 (make-mobile
                        (make-branch 3 (make-mobile
                                         (make-branch 3 
                                                      (make-mobile
                                                        (make-branch 4 3)
                                                        (make-branch 3 4)))
                                         (make-branch 3 12)))
                        (make-branch 3 (make-mobile
                                         (make-branch 3 144)
                                         (make-branch 3 
                                                      (make-mobile
                                                        (make-branch 3 4)
                                                        (make-branch 4 3)))))))

(deftest binary-mobile-test
  (testing "calculating total weight"
    (let [simplest-mobile (make-mobile (make-branch 3 11)
                                       (make-branch 3 12)) 
          deeper-right-branch (make-mobile
                               (make-branch 1 10)
                               (make-branch 1
                                            (make-mobile (make-branch 3 10)
                                                         (make-branch 3 10))))]
      (is (= 23 (total-weight simplest-mobile)))
      (is (= 30 (total-weight deeper-right-branch)))
      (is (= 10 (total-weight (make-mobile 
                               (make-branch 1 5) 
                               (make-branch 1 5)))))))
  (testing "torque calculation"
    (let [simple-branch-mobile (make-mobile
                                (make-branch 1 5)
                                (make-branch 1 8))
          deeper-branching (make-mobile
           (make-branch 1 44)
           (make-branch 7 (make-mobile
                           (make-branch 1 5)
                           (make-branch 1 6))))]
      (is (= 5 (torque (left-branch simple-branch-mobile))))
      (is (= 8 (torque (right-branch simple-branch-mobile))))
      (is (= 44 (torque (left-branch deeper-branching))))
      (is (= 77 (torque (right-branch deeper-branching))))))
  (testing "balancing checks"
      (let [basic-balanced (make-mobile
                            (make-branch 3 11)
                            (make-branch 3 11))
            basic-unbalanced (make-mobile
                              (make-branch 3 11)
                              (make-branch 3 12))
            ;Not the torque of the hanging rod 
            ;does not include their torques, 
            ;only the total weight
            deeper-balanced (make-mobile
                             (make-branch 2 22)
                             (make-branch 2 (make-mobile
                                             (make-branch 2 11)
                                             (make-branch 2 11))))]
        (is (balanced? basic-balanced))
        (is (not (balanced? basic-unbalanced)))
        (is (balanced? deeper-balanced))
        )))