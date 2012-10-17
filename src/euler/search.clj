(ns euler.search)


(defn truncated-half-count [items]
  (int (/ (count items) 2)))

(defn middle-index [items]
  (truncated-half-count items))

(defn middle-item [items]
  (nth items (middle-index items)))

(defn drop-half [items]
  (drop (truncated-half-count items) items))

(defn take-half [items]
  (take (truncated-half-count items) items))

(defn new-items [items function target]
  (let [middle-value (function (middle-item items))]
    (cond
      (= middle-value target)
      (list (middle-item items))
      (> middle-value target)
      (take-half items)
      (< middle-value target)
      (drop-half items))))

(defn binary-search [items function target]
  (if (= 1 (count items))
    (first items)
    (binary-search (new-items items function target) function target)
  ))

