(ns euler.58-test
  (:use clojure.test
        euler.58))

(deftest ratios
  (testing "radius 3"
    (is (=
          (diag-prime-ratio 3)
          (/ 8 13)))))


