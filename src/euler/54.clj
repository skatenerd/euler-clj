(ns euler.54)

(declare has-straight-flush has-flush has-straight has-two-pair who-wins has-highest-card has-pair pair-winner pair-card has-triple triple-card has-full-house has-quadruple quadruple-card)

(def handz
  [{:hand :straight-flush
    :priority 9
    :detector has-straight-flush}
   {:hand :quadruple
    :priority 8
    :detector has-quadruple}
   {:hand :full-house
    :priority 7
    :detector has-full-house}
   {:hand :flush
    :priority 6
    :detector has-flush}
   {:hand :straight
    :priority 5
    :detector has-straight}
   {:hand :triple
    :priority 4
    :detector has-triple}
   {:hand :two-pair
    :priority 3
    :detector has-straight}
   {:hand :pair
    :priority 2
    :detector has-pair}
   {:hand :straight
    :priority 1
    :detector (fn [] true)}])

(def category-hierarchy
  {:highest-card 1
   :pair 2
   :two-pair 3
   :triple 4
   :straight 5 
   :flush 6
   :full-house 7
   :quadruple 8
   :straight-flush 9})

(defn hand-summary [hand composing-card]
  {:hand hand :composing-card composing-card})

(defn sort-descending [hand]
  (sort #(> %1 %2) hand))

(defn get-highest-hand [hand]
  (let [hand-numbers (map :number hand)]
  (cond
    (has-straight-flush hand hand-numbers) (hand-summary :straight-flush (apply max hand-numbers))
    (has-quadruple hand-numbers) (hand-summary :quadruple (quadruple-card hand-numbers))
    (has-full-house hand-numbers) (hand-summary :full-house (apply max hand-numbers))
    (has-flush hand) (hand-summary :flush (apply max hand-numbers))
    (has-straight hand-numbers) (hand-summary :straight (apply max hand-numbers))
    (has-triple hand-numbers) (hand-summary :triple (triple-card hand-numbers))
    (has-two-pair hand-numbers) (hand-summary :two-pair (pair-card hand-numbers))
    (has-pair hand-numbers) (hand-summary :pair (pair-card hand-numbers))
    :else (hand-summary :highest-card (apply max hand-numbers)))))

(defn analyze-hand [hand]
  {:sorted-cards (sort-descending (map :number hand))
   :highest-hand (get-highest-hand hand)})

(defn category-beats [first-category second-category]
  (> (first-category category-hierarchy)
     (second-category category-hierarchy)))

(defn category-winner [first-hand second-hand]
  (who-wins #(category-beats
               (:hand (:highest-hand %1))
               (:hand (:highest-hand %2)))
            first-hand
            second-hand))

(defn composing-card-winner [first-hand second-hand]
  (who-wins #(>
               (:composing-card (:highest-hand %1))
               (:composing-card (:highest-hand %2)))
            first-hand
            second-hand))

(defn hand-based-winner [first-hand second-hand]
  (or (category-winner first-hand second-hand)
    (composing-card-winner first-hand second-hand)))


(defn winner [first-hand second-hand]
  (let [first-hand-analyzed (analyze-hand first-hand)
        second-hand-analyzed (analyze-hand second-hand)]
    (or
      (hand-based-winner first-hand-analyzed second-hand-analyzed)
      (has-highest-card (:sorted-cards first-hand-analyzed) (:sorted-cards second-hand-analyzed)))))

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

(defn has-pair [hand]
  (not (nil? (pair-card hand))))

(defn has-two-pair [hand]
  (let [occurrences (occurrences-hash hand) ]
    (= 2 (count (set (filter #(= 2 (get occurrences %))
                         hand))))))

(defn has-triple [hand]
  (not (nil? (triple-card hand))))

(defn has-quadruple [hand]
  (not (nil? (quadruple-card hand))))

(defn has-straight [hand]
  (= (sort hand) (range (apply min hand) (inc (apply max hand)))))

(defn has-flush [hand]
  (= 1 (count (set (map :suit hand)))))

(defn has-straight-flush [hand hand-numbers]
  (and (has-flush hand)
       (has-straight hand-numbers)))

(defn has-full-house [hand]
  (let [occurrences (occurrences-hash hand)]
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
