(ns euler.prime-test
  (:use clojure.test
        euler.prime))

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

(deftest check-prime
  (testing "1"
    (is (not (prime? 1))))
  (testing "2"
    (is (prime? 2)))
  (testing "3"
    (is (prime? 3)))
  (testing "4"
    (is (not (prime? 4))))
  (testing "5"
    (is (prime? 5)))
  (testing "6"
    (is (not (prime? 6))))
  (testing "7"
    (is (prime? 7))))

(deftest problem
  (testing "problem"
    (is (miller-rabin 31 10))))

(deftest mill-rabin-speed
  (testing "big"
    (is (miller-rabin 611953 10)))
         )

(deftest miller-rabin-check
  (testing "all"
    (doall
      (map
        #(testing (str "miller rabin " %) (is (= (prime? %) (miller-rabin % 4))))
        (range 4 5000)))))
