(ns euler.60-test
  (:use clojure.test
        euler.60
        euler.prime
        ))

(deftest pairs-for-thre
   (testing "gets pairs"
        (is (= #{[:a :b]
                 [:b :a]
                 [:a :c]
                 [:c :a]
                 [:b :c]
                 [:c :b]
                 }
               (pairs #{:a :b :c})))))

(deftest
  concatenates-numbers
  (testing "concatenation"
           (is (= 3456  (concat-numbers 34 56)))))

(deftest
  remarkable-primes
  (testing "the example from the problem"
           (is (remarkable-primes? #{3 7 109 673} (primes-under 1000)))
           (is (not (remarkable-primes? [2 2 2 2 2] (primes-under 10))))

           )

  (testing "the answer"
           (prn (find-it 1000))
           (prn "?")
           (prn (find-it 10000))
           (prn "?")
           (prn (find-it 100000))
           (prn "?")
           (prn (find-it 1000000))
           (prn "?")
           (prn (find-it 10000000))
           (prn "?")
           (prn (find-it 100000000))
           (prn "?")
           )
  )
