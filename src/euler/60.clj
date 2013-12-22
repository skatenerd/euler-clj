(ns euler.60
  (:use euler.prime))

(defn- crude-log [target base]
  (let [raises (iterate #(* base %) 1)]
    (first (keep-indexed #(if (> %2 target) %1) raises))))

(defn pairs [unique-elements]
  (set (for [first-element unique-elements
             second-element unique-elements
             :when (not (= first-element second-element))]
         [first-element second-element])))

(defn concat-numbers [head tail]
  (let [exponent (crude-log tail 10)
        raised (apply * head (repeat exponent 10)) ]

    (+ raised tail)))

(def fast-concat-numbers (memoize concat-numbers))


(defn remarkable-primes? [primes primes-reservoir]
  (and
    (every? #(prime? % primes-reservoir) (map #(apply concat-numbers %) (pairs primes)))))


(defn dfs [node neighbors predicate]
  (if (predicate node)
    node
    (some #(dfs % neighbors predicate) (neighbors node))))

(def fast-prime (memoize prime?))

(defn composable? [p1 p2]
  (and
    (fast-prime (fast-concat-numbers p1 p2))
    (fast-prime (fast-concat-numbers p2 p1))))

(defn find-interesting-primes [upper-boundary length]
  (let [primes (primes-under upper-boundary)]
    (dfs
      #{}
      (fn [interesting-so-far]
        (let [addable-stuff
              (filter
                (fn [candidate-prime]
                  (every?
                    #(composable? candidate-prime %)
                    interesting-so-far))
                primes)]
          (map #(conj interesting-so-far %) addable-stuff)
          ))
      #(= length (count %)))))
