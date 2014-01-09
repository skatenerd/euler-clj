(ns euler.challenge
  (:use euler.dfs))

(def numbers-to-letters
  {2 ["a" "b" "c"]
   3 ["d" "e" "f"]
   4 ["g" "h" "i"]
   5 ["j" "k" "l"]
   6 ["m" "n" "o"]
   7 ["p" "q" "r" "s"]
   8 ["t" "u" "v"]
   9 ["w" "x" "y" "z"]})

(def word-set
  (into
    #{}
    (clojure.string/split
      (slurp "src/euler/word_list.txt")
      #"\r\n")))

(defn potential-strings [numbers]
  (set (map :letters-so-far
    (dfs-all
      {:letters-so-far "" :remaining-numbers numbers}
      (fn [node]
        (let [rest-numbers (rest (:remaining-numbers node))
              letters-to-add (get numbers-to-letters (first (:remaining-numbers node)))]
          (map (fn [letter] {:letters-so-far (str (:letters-so-far node) letter) :remaining-numbers rest-numbers}) letters-to-add)))
      #(empty? (:remaining-numbers %))))))


(defn drop-string [to-drop string]
  (clojure.string/replace-first string to-drop ""))

(defn prefixes [string]
  (map #(apply str (take % string)) (range 1 (inc (count string)))))

(defn made-of-words? [string]
  (dfs
    string
    (fn [node]
      (let [prefixes (prefixes node)
            word-prefixes (filter #(contains? word-set %) prefixes)
            ]
        (map #(drop-string % node) word-prefixes)))
    empty?))

(defn word-breakdowns [numbers]
  (filter made-of-words? (potential-strings numbers)))
