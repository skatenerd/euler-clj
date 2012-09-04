(ns euler.55-test
  (:use clojure.test
        euler.55))

(deftest lychrel
  (testing "47"
    (is (not (lychrel-50? 47))))
  (testing "349"
    (is (not (lychrel-50? 349))))
  (testing "196"
    (is (lychrel-50? 196 50)))
  (testing "10677"
    (is (lychrel-50? 10677 50)))
         )
