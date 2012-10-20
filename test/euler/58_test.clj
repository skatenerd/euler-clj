(ns euler.58-test
  (:use clojure.test
        euler.58))

(deftest ratios
  (testing "radius 3"
    (is (=
          (ratio-for-statistic (nth diag-prime-statistics 2))
          (/ 8 13)))))


