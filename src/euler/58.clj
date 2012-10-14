(ns euler.58
  (:use clojure.set))
(declare primes-under)
(defn next-diags [prev-diags]
  (let [difference (+ 2 (- (second prev-diags) (first prev-diags)))
        first-diag (+ difference (last prev-diags))]
    [first-diag (+ difference first-diag) (+ difference difference first-diag) (+ difference difference difference first-diag)]))
(def diagonalerator (iterate next-diags [3 5 7 9]))
(defn diags-for-radius [radius] (cons 1 (flatten (take radius diagonalerator))))



(defn divides? [num denom] (= 0 (mod num denom)))
(defn strict-divides? [num denom]
  (and
    (> num denom)
    (divides? num denom)))

(defn diag-prime-ratio [radius]
  (let [diagonals (diags-for-radius radius)
        _ (prn (apply max diagonals))
        primes (primes-under (apply max diagonals))
        _ (prn (apply max diagonals))
        number-of-primes (count (intersection (set diagonals) (set primes)))]
    (/ number-of-primes (count diagonals))))

(defn continue-sieve? [sieve-index current-sieve]
  (< sieve-index (count current-sieve)))

(defn filter-sieve [sieve-index current-sieve]
  (let [prime-at-index (nth current-sieve sieve-index)]
  (remove
     #(strict-divides? % (nth current-sieve sieve-index))
     current-sieve)))

(defn primes-under
  ([limit]
  (primes-under limit 0 (range 2 limit)))
  ([limit sieve-index current-sieve]
  (if (continue-sieve? sieve-index current-sieve)
    (primes-under limit (inc sieve-index) (filter-sieve sieve-index current-sieve))
    current-sieve)))
