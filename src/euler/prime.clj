(ns euler.prime)


(defn divides? [num denom] (= 0 (mod num denom)))


(defn relevant-tries [number]
  (let [limit (+ 2 (int (Math/sqrt number)))]
    (if (> number 2)
      (conj (range 3 limit 2) 2)
      [])))

(defn prime-candidate [candidate primes]
  (not-any? #(divides? candidate %)
            primes))

(defn next-prime [primes]
  (conj primes
        (first (filter
           #(prime-candidate % primes)
           (iterate inc (apply max primes))))))

(def lazy-primes
  (iterate next-prime [2]))

(defn primes-under [limit]
  (drop-last (first (filter #(<= limit (apply max %)) lazy-primes))))

(defn prime? [number]
  (and (> number 1)
       (not-any? #(divides? number %) (relevant-tries number))))
