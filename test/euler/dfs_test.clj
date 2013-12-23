(ns euler.60-test
  (:use clojure.test
        euler.dfs))
(deftest
  dfs-test
  (testing
    "dfs"
    (let [graph {1 [2 3 99999]
                 2 [4]
                 3 [8]
                 4 [11]}
          root 1
          neighbors #(get graph % [])
          predicate #(> % 10)]
      (is (= 11 (dfs root neighbors predicate))))))
