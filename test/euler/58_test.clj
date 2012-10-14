(ns euler.58-test
  (:use clojure.test
        euler.58))

(deftest ratios
  (testing "radius 3"
    (is (=
          (diag-prime-ratio 3)
          (/ 8 13)))))

(deftest primes
  (testing "primes under 10"
    (is (=
          [2 3 5 7]
          (primes-under 10))))
  (testing "primes under 11"
    (is (=
          [2 3 5 7]
          (primes-under 11))))
  (testing "primes under 12"
    (is (=
          [2 3 5 7 11]
          (primes-under 12)))))
