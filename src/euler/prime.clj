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

(defn safe-pow-mod [base exp modulus]
  (reduce
    (fn [agg current]
      (mod (* agg current) modulus))
    1
    (repeat exp base)))

(def lazy-primes
  (iterate next-prime [2]))

(defn primes-under [limit]
  (drop-last (first (filter #(<= limit (apply max %)) lazy-primes))))

(defn prime? [target]
  (and (> target 1)
       (not-any? #(divides? target %) (relevant-tries target))))

(defn get-s-factor [target]
  (dec (first-predicate
     #(not (divides? (dec target) (Math/pow 2 %)))
     (iterate inc 0))))

(defn get-d-factor [target]
  (/ (dec target) (Math/pow 2 (get-s-factor target))))

(defn rand-in-range [low high]
  (+ low (Math/floor (rand (- high low)))))

(defn compact [values]
  (filter #(not (nil? %)) values))

(defn composite-in-inner-loop [target computed]
  (if (or (= 1.0 computed) (= (dec target) computed))
    (= 1.0 computed)
    nil))

(defn composite-for-inner-loop [s-factor x target]
  (let [computeds (take
                    (max (dec s-factor) 1)
                    (drop 1 (iterate
                      #(mod (* % %) target)
                      x)))
        indicators (map #(composite-in-inner-loop target %) computeds)
        compacted (compact indicators)
        first-bool (first compacted)]
    (if (or first-bool (nil? first-bool))
      true
      false)))

(defn composite-for-iteration [s-factor d-factor target]
  (let [random-choice (rand-in-range 2 (dec target))
        x (safe-pow-mod random-choice d-factor target)]
    (if (or (= x 1.0) (= x (dec target)))
      false
      (composite-for-inner-loop s-factor x target))))

(defn miller-rabin [target iterations]
  (if (and (odd? target) (< 1 target))
    (let [s-factor (get-s-factor (double target))
          d-factor (get-d-factor (double target))]
      (not-any? (fn [_] (composite-for-iteration s-factor d-factor (double target))) (range iterations)))
    false))

