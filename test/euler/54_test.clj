(ns euler.54-test
  (:use clojure.test
        euler.54))

(defn build-card
  ([number]
  {:number number
   :suit :clubs})
  ([number suit]
  {:number number
   :suit suit}))

(defn numbers-to-cards [numbers]
  (let [card-descriptions (vec (map build-card numbers))]
    (assoc-in card-descriptions [0 :suit] :hearts)))

(deftest higest-card
  (testing "highest card wins"
    (is (= :first (winner (numbers-to-cards [15 13 4 10 5])
                          (numbers-to-cards [15 13 5 9 4])))))
  (testing "highest card wins"
    (is (= :second (winner (numbers-to-cards [15 13 9 5 4])
                           (numbers-to-cards [15 13 10 5 4])))))
  (testing "equal cards no winner"
    (is (= nil (winner (numbers-to-cards [15 13 9])
                       (numbers-to-cards [15 13 9])))))
         )

(deftest pair-beats-highest-card
  (testing "pair of twos vs highest 8"
    (is (= :first (winner (numbers-to-cards [6 5 2 2 1])
                          (numbers-to-cards [8 4 3 2 1])))))
  (testing "highest 8 loses to pair of twos"
    (is (= :second (winner (numbers-to-cards [8 4 3 2 1])
                           (numbers-to-cards [6 5 2 2 1])))))
  (testing "tie with pair"
    (is (= nil (winner (numbers-to-cards [6 5 2 2 1])
                       (numbers-to-cards [6 5 2 2 1])))))
         )

(deftest pair-beats-pair
  (testing "pair of fives beats pair of fours"
    (is (= :first (winner (numbers-to-cards [6 5 5 2 1])
                          (numbers-to-cards [9 4 4 2 1])))))
         )

(deftest triple-beats-triple
  (testing "triple beats a triple"
    (is (= :first (winner (numbers-to-cards [3 3 3 4 5])
                          (numbers-to-cards [2 2 2 12 13]))))))

(deftest triple-beats-pair
  (testing "thre twos beats pair of fours"
    (is (= :first (winner (numbers-to-cards [6 2 2 2 1])
                          (numbers-to-cards [9 4 4 2 1])))))
         )

(deftest full-house-beats-triple
  (testing "lame full house beats triple"
    (is (= :first (winner (numbers-to-cards [2 2 2 1 1]) 
                          (numbers-to-cards [9 4 4 4 1])))))
         )

(deftest quadruple-beats-full-house
  (testing "lame full house beats triple"
    (is (= :first (winner (numbers-to-cards [2 2 2 2 1]) 
                          (numbers-to-cards [4 4 4 1 1])))))
         )

(deftest two-pair
  (testing "two pair beats pair"
    (is (= :first (winner (numbers-to-cards [4 4 2 2 1]) 
                          (numbers-to-cards [8 8 3 2 1]))))))

(deftest straight
  (testing "straight beats triple"
    (is (= :first (winner (numbers-to-cards [1 2 3 4 5])
                          (numbers-to-cards [8 8 8 2 1])))))

  (testing "straight beats straight"
    (is (= :second (winner (numbers-to-cards [5 4 3 2 1]) 
                           (numbers-to-cards [8 7 6 5 4]))))))

(deftest test-flush
  (testing "flush beats straight"
    (let [flush-hand (map #(build-card % :clubs) [9 5 3 2 1])]
    (is (= :first (winner flush-hand
                          (numbers-to-cards [1 2 3 4 5]))))))

  (testing "royal flush beats flush"
    (let [flush-hand (map #(build-card % :clubs) [9 5 3 2 1])
          royal-flush-hand (map #(build-card % :hearts) [14 13 12 11 10])]
    (is (= :second (winner flush-hand
                           royal-flush-hand))))))
