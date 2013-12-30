(ns euler.62
  (:use euler.prime
        clojure.set
        euler.dfs))

(defn permutations [items]
  (filter #(= (count items) (count %))
          (tree-seq
            #(< (count %) (count items))
            (fn [items-so-far]
              (let [total-frequences (frequencies items)
                    so-far-frequencies (frequencies items-so-far)
                    remaining-items (map key (filter (fn [[k v]] (> v (get so-far-frequencies k 0))) total-frequences))]
                (map #(conj items-so-far %) remaining-items)))
            []
            ))
  )

(defn- digits [number]
  (map #(Integer/parseInt (str %)) (seq (str number))))

(def cubes (map #(* % % %) (range 1 10000)))

(def cube-bucket
  (reduce
    (fn [bucket current]
      (update-in bucket [(sort (digits current))] (fnil #(conj % current) #{})))
    {}
    cubes))

(def candidates
  (filter (fn [[k v]] (= 5 (count v))) cube-bucket))

(def the-answer (map #(apply min (val %)) candidates))
