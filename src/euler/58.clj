(ns euler.58
  (:use euler.prime))

(defn next-diags [prev-diags]
  (let [difference (+ 2 (- (second prev-diags) (first prev-diags)))
        first-diag (+ difference (last prev-diags))]
    [first-diag (+ difference first-diag) (+ difference difference first-diag) (+ difference difference difference first-diag)]))

(defn next-diag-prime-statistic [old]
  (let [new-diagonals (next-diags (:diagonals-at-radius old))
        discovered-primes (filter prime? new-diagonals)
        new-total-primes (+ (count discovered-primes) (:total-primes old))]
    {:radius (inc (:radius old))
     :total-primes new-total-primes
     :diagonals-at-radius new-diagonals
     :total-numbers (+ 4 (:total-numbers old))}))



(def diag-prime-statistics
  (iterate
    next-diag-prime-statistic
    {:radius 1
     :total-primes 3
     :diagonals-at-radius [3 5 7 9]
     :total-numbers 5}))

(defn ratio-for-statistic [statistic]
  (/ (:total-primes statistic) (:total-numbers statistic)))




(defn answer []
  (first (filter #(> 0.1 (ratio-for-statistic %)) diag-prime-statistics)))
