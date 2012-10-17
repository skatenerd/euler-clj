(ns euler.58
  (:use euler.prime))

(defn next-diags [prev-diags]
  (let [difference (+ 2 (- (second prev-diags) (first prev-diags)))
        first-diag (+ difference (last prev-diags))]
    [first-diag (+ difference first-diag) (+ difference difference first-diag) (+ difference difference difference first-diag)]))
(def diagonalerator (iterate next-diags [3 5 7 9]))
(defn diags-for-radius [radius] (cons 1 (flatten (take radius diagonalerator))))

(defn diag-prime-ratio [radius]
  (let [diagonals (diags-for-radius radius)
        number-of-primes (count (filter prime? diagonals))]
    (/ number-of-primes (count diagonals))))

