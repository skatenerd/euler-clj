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
           (is (not (remarkable-primes? #{2 3 5 7 11} (primes-under 10))))

           )

  )

(deftest
  dfs-test
  (testing
    "dfs"
    (prn (composable-pairs #{2 3 5 7}))
    (let [graph {1 [2 3 99999]
                 2 [4]
                 3 [8]
                 4 [11]}
          root 1
          neighbors #(get graph % [])
          predicate #(> % 10)]
      (is (= 11 (dfs root neighbors predicate))))))

(deftest
  finding-interesting-primes
  (is (= #{3 7 109 673} (find-interesting-primes 900 4)))
  ;(prn (find-interesting-primes 10000 5))
  )
