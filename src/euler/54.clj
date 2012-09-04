(ns euler.54
  (:require [clojure.string :as string]))

(declare categoriez highest-satisfied-category has-straight-flush has-flush has-straight has-two-pair who-wins has-highest-card has-pair pair-winner pair-card has-triple triple-card has-full-house has-quadruple quadruple-card)


(defn hand-summary [hand composing-card]
  {:hand hand :composing-card composing-card})

(defn sort-descending [items]
  (sort #(> %1 %2) items))

(defn sorted-cards [hand]
  (sort-descending (map :number hand)))

(defn associate-composing-card [hand hand-numbers]
  (let [composing-card-fn (:composing-card hand)]
    (assoc 
      hand 
      :composing-card 
      (composing-card-fn hand-numbers))))

(defn satisfied [configuration hand hand-numbers]
 (let [detector (:detector configuration)]
   (detector hand hand-numbers)))

(defn highest-satisfied-category [hand]
  (let [hand-numbers (map :number hand)
        categories-satisfied (filter #(satisfied % hand hand-numbers) categoriez)
        first-category-satisfied (first categories-satisfied) ]
    (associate-composing-card
      first-category-satisfied
      hand-numbers)))

(defn analyze-hand [hand]
  (highest-satisfied-category hand))

(defn category-winner [first-hand-summary second-hand-summary]
  (who-wins #(>
               (:priority %1)
               (:priority %2))
            first-hand-summary
            second-hand-summary))

(defn category-tie-breaker [first-hand-summary second-hand-summary]
  (who-wins #(>
               (:composing-card %1)
               (:composing-card %2))
            first-hand-summary
            second-hand-summary))

(defn category-based-winner [first-hand second-hand]
  (or (category-winner first-hand second-hand)
    (category-tie-breaker first-hand second-hand)))

(defn winner [first-hand second-hand]
  (let [first-hand-analyzed (analyze-hand first-hand)
        second-hand-analyzed (analyze-hand second-hand)]
    (or
      (category-based-winner first-hand-analyzed second-hand-analyzed)
      (has-highest-card (sorted-cards first-hand)
                        (sorted-cards second-hand)))))

(defn who-wins [score-fn first-hand second-hand]
  (if (score-fn first-hand second-hand)
    :first
    (if (score-fn second-hand first-hand)
      :second
      nil)))

(defn increment-occurrences [occurrences card]
  (let [current-count (get occurrences card)
        current-count (or current-count 0)
        new-count (inc current-count)
        with-incremented (assoc occurrences card new-count)]
    with-incremented
    ))

(defn occurrences-hash 
  ([hand]
    (occurrences-hash {} hand))
  ([occurrences hand]
    (if (empty? hand)
      occurrences
      (recur (increment-occurrences occurrences (first hand))
             (rest hand)))))

(defn safe-max [list-of-numbers]
  (if (empty? list-of-numbers)
    nil
    (apply max list-of-numbers)))

(defn pair-card [hand]
  (let [occurrences (occurrences-hash hand)]
    (safe-max (filter #(= 2 (get occurrences %))
                       hand))))
(defn triple-card [hand]
  (let [occurrences (occurrences-hash hand)]
    (safe-max (filter #(= 3 (get occurrences %))
                       hand))))

(defn quadruple-card [hand]
  (let [occurrences (occurrences-hash hand)]
    (safe-max (filter #(= 4 (get occurrences %))
                       hand))))

(defn has-pair [_ hand-numbers]
  (not (nil? (pair-card hand-numbers))))

(defn has-two-pair [_ hand-numbers]
  (let [occurrences (occurrences-hash hand-numbers) ]
    (= 2 (count (set (filter #(= 2 (get occurrences %))
                         hand-numbers))))))

(defn has-triple [_ hand-numbers]
  (not (nil? (triple-card hand-numbers))))

(defn has-quadruple [_ hand-numbers]
  (not (nil? (quadruple-card hand-numbers))))

(defn has-straight [_ hand-numbers]
  (= (sort hand-numbers) (range (apply min hand-numbers) (inc (apply max hand-numbers)))))

(defn has-flush [hand _]
  (= 1 (count (set (map :suit hand)))))

(defn has-straight-flush [hand hand-numbers]
  (and (has-flush hand hand-numbers)
       (has-straight hand hand-numbers)))

(defn has-full-house [_ hand-numbers]
  (let [occurrences (occurrences-hash hand-numbers)]
    (and (= 3 (apply max (vals occurrences)))
         (= 2 (apply min (vals occurrences))))))

(defn has-highest-card [first-hand second-hand]
  (if (empty? first-hand)
    nil
    (if (= (first first-hand) (first second-hand))
      (recur (rest first-hand) (rest second-hand))
      (who-wins #(> (first %1) (first %2))
                first-hand
                second-hand))))
(def categoriez
  [{:hand :straight-flush
    :priority 9
    :detector has-straight-flush
    :composing-card #(apply max %)}
   {:hand :quadruple
    :priority 8
    :detector has-quadruple
    :composing-card #(quadruple-card %)}
   {:hand :full-house
    :priority 7
    :detector has-full-house
    :composing-card #(apply max %)}
   {:hand :flush
    :priority 6
    :detector has-flush
    :composing-card #(apply max %)}
   {:hand :straight
    :priority 5
    :detector has-straight
    :composing-card #(apply max %)}
   {:hand :triple
    :priority 4
    :detector has-triple
    :composing-card #(triple-card %)}
   {:hand :two-pair
    :priority 3
    :detector has-two-pair
    :composing-card #(pair-card %)}
   {:hand :pair
    :priority 2
    :detector has-pair
    :composing-card #(pair-card %)}
   {:hand :highest-card
    :priority 1
    :detector (fn [_ _] true)
    :composing-card #(apply max %)}])

(def number-parse-lookup
  {
   "1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9
   "T" 10
   "J" 11
   "Q" 12
   "K" 13
   "A" 14
   })

(def suit-parse-lookup
  {
   "C" :clubs
   "S" :spades
   "H" :hearts
   "D" :diamonds
   })

(defn parse-card [card-string]
  (let [number-string (str (first card-string))
        suit-string (str (second card-string))]
    {
     :number (get number-parse-lookup number-string)
     :suit (get suit-parse-lookup suit-string)
     }))

(defn parse-hand [hand-string]
  (map parse-card hand-string))

(defn parse-line [line]
  (let [splitted (string/split line #" ")
        first-hand (take 5 splitted)
        second-hand (drop 5 splitted)]
    [(parse-hand first-hand)
     (parse-hand second-hand)]))


(defn -main []
  (with-open [rdr (clojure.java.io/reader "src/euler/poker.txt")]
    (reduce
      (fn [total current-line]
        (let [winner (apply winner (parse-line current-line))]
          (if (= :first winner)
            (inc total)
            total)))
      0
      (line-seq rdr))))
