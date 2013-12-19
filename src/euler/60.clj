(ns euler.60
  (:use euler.prime))

(defn- crude-log [target base]
  (let [raises (iterate #(* base %) 1)]
    (first (keep-indexed #(if (> %2 target) %1) raises))
    ))


(defn pairs [elements]
  (let [with-indices (keep-indexed #(identity [%2 %1]) elements)]
    (set
      (for [first-element with-indices
            second-element with-indices
            :when (not (= first-element second-element)) ]
        [(first first-element) (first second-element)]))))


(defn concat-numbers [head tail]
  (let [exponent (crude-log tail 10)
        raised (apply * head (repeat exponent 10)) ]

    (+ raised tail)))

(defn remarkable-primes? [primes]
  (and
    (every? prime? (map #(apply concat-numbers %) (pairs primes)))))

(defn find-it [limit]
  (let [primes-reservoir (primes-under limit)]
    (set
      (for [p1 primes-reservoir
            p2 primes-reservoir
            p3 primes-reservoir
            p4 primes-reservoir
            p5 primes-reservoir]
        (if (remarkable-primes? [p1 p2 p3 p4 p5])
          (do
            (prn [p1 p2 p3 p4 p5])
            (set [p1 p2 p3 p4 p5])))))))
