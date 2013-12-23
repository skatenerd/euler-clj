(ns euler.dfs)

(defn dfs [node neighbors predicate]
  (if (predicate node)
    node
    (some #(dfs % neighbors predicate) (neighbors node))))
