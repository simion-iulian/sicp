(ns chapter2.exercise2-29-test
  (:require [clojure.test :refer :all]
            [chapter2.exercise2-29 :refer :all]))

(def mobile1 (make-mobile
              (make-branch 3 11)
              (make-branch 3 12)))

(def big-mobile
  (make-mobile
   (make-branch 1 10)
   (make-branch 1
                (make-mobile (make-branch 3 10)
                             (make-branch 3 10)))))

(def balanced-mobile (make-mobile
                      (make-branch 3 11)
                      (make-branch 3 11)))

(def balanced-mobile2 (make-mobile
                       (make-branch 3 (make-mobile
                                       (make-branch 3 23)
                                       (make-branch 3 23)))
                       (make-branch 3 (make-mobile
                                       (make-branch 3 23)
                                       (make-branch 3 23)))))

(def unbalanced-mobile (make-mobile
                        (make-branch 3 11)
                        (make-branch 3 12)))

(def balanced-mobile2 (make-mobile
                         (make-branch 3 (make-mobile
                                         (make-branch 3 11)
                                         (make-branch 3 12)))
                         (make-branch 3 (make-mobile
                                         (make-branch 3 12)
                                         (make-branch 3 11)))))

(def unbalanced-mobile3 
  (make-mobile
   (make-branch 3 888)
   (make-branch 3 (make-mobile
                   (make-branch 3 12)
                   (make-branch 3 11)))))

(deftest binary-mobile-test
  (testing "calculating total weight"
    (is (= 23 (total-weight mobile1)))
    (is (= 30 (total-weight big-mobile))))
  (testing "torque and balancing"
    (is (balanced? balanced-mobile2))
    (is (not (balanced? unbalanced-mobile)))
    (is (balanced? balanced-mobile2))
    (is (not (balanced? unbalanced-mobile3)))))