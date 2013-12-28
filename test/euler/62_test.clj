(ns euler.62-test
  (:use clojure.test
        euler.62
        euler.prime))


(deftest foo
  (testing "permutations"
           (is (= #{[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]} (set (permutations [1 2 3]))))
           (is (= #{[1 2 2] [2 1 2] [2 2 1]} (set (permutations [1 2 2]))))


           )
         )
