(ns euler.dfs-test
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
      (is (= 11 (dfs root neighbors predicate)))))

  (testing
    "dfs-all finds all of the dfs hits"
    (let [graph {1 [2 3 99999]
                 2 [4]
                 3 [8]
                 4 [11]}
          root 1
          neighbors #(get graph % [])
          predicate #(> % 7)]
      (is (= #{99999 11 8} (dfs-all root neighbors predicate))))))
