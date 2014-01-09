(ns euler.dfs
  (:use clojure.set))

(defn dfs [node neighbors predicate]
  (if (predicate node)
    node
    (some #(dfs % neighbors predicate) (neighbors node))))

(defn dfs-all
  ([node neighbors predicate]
   (dfs-all node neighbors predicate #{}))
  ([node neighbors predicate finds]
   (let [new-finds (if (predicate node)
                     (conj finds node)
                     finds)]
     (reduce
       union
       new-finds
       (map #(dfs-all % neighbors predicate) (neighbors node)))))


  )
