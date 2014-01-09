(ns euler.dfs
  (:use clojure.set))

(defn dfs [node neighbors predicate]
  (if (predicate node)
    node
    (some #(dfs % neighbors predicate) (neighbors node))))

(defn dfs-all
  ([node neighbors predicate]
   (let [new-finds (if (predicate node)
                     #{node}
                     #{})]
     (reduce
       union
       new-finds
       (map #(dfs-all % neighbors predicate) (neighbors node))))))
