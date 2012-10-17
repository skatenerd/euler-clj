(ns euler.search-test
  (:use clojure.test
        euler.search))

(is (= 3 (binary-search (range 10) identity 3)))
(is (= 3 (binary-search (range 11) identity 3)))
(is (= 2 (binary-search (range 20) #(* 2 %) 4)))
(is (= 2 (binary-search (range 21) #(* 2 %) 4)))
