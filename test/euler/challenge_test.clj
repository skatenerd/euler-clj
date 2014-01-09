(ns euler.challenge-test
  (:use clojure.test
        euler.challenge))


(deftest potential-strings-test
  (is (contains? (potential-strings [8 7 3 7 8 2 9]) "useruby")))

(deftest scoring
  (is (made-of-words? "useruby"))
  (is (not (made-of-words? "flbzrrr")))
         )

(deftest integration
  (is (= ["useruby"] (word-breakdowns [8 7 3 7 8 2 9]))))
