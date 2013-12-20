(ns euler.prime)

(defn first-predicate [predicate tries]
  (first (filter predicate tries)))

(defn divides? [num denom] (= 0.0 (double (mod num denom))))


(defn relevant-tries [target]
  (let [limit (+ 2 (int (Math/sqrt target)))]
    (if (> target 2)
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

(defn prime?
  ([target primes]
   (let [limit (inc (int (Math/sqrt target)))
         primes (take-while #(<= % limit) primes)]
     (and (> target 1)
       (not-any? #(divides? target %) primes))))
  ([target]
  (prime? target (relevant-tries target))))

