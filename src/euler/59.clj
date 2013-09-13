(ns euler.59)

(def word-set
  (into
    #{}
    (clojure.string/split
      (slurp "src/euler/word_list.txt")
      #"\r\n")))

(def cipher-file
  (map #(-> % clojure.string/trim Integer/parseInt)
       (clojure.string/split
         (slurp "src/euler/cipher59.txt")
         #",")))

(defn unbitify [s]
  (let [pows-of-2 (keep-indexed #(if (= 1 %2) %1) (reverse s))]
    (apply + (map #(int (Math/pow 2 %)) pows-of-2))))


(defn pad-to [thing length to-use]
  (let [deficite (- length (count thing))
        moar (repeat deficite to-use)]
   (concat moar thing)))

(defn bitify [c]
  (let [short-seq-of-strings (clojure.string/split (Integer/toBinaryString c) #"")
        trimmed (rest short-seq-of-strings)
        numz (map #(Integer/parseInt %) trimmed)]
(pad-to numz 8 0)))

(defn char-for-i [i]
  (first (seq (Character/toString (char i)))))

(defn char-for-bits [b]
  (char-for-i (unbitify b)))

(defn bits-for-char [c]
  (bitify (int c)))

(defn xor [a b]
  (if (= a b) 0 1))

(defn cipher [text cipher-key]
 (let [cipher-bits (map bits-for-char cipher-key)
       repeated-cipher (mapcat identity (repeat cipher-bits))
       text-bits (map bits-for-char text)
       ciphered-bits (map #(map xor %1 %2) text-bits repeated-cipher)]
    (map char-for-bits ciphered-bits)))

(defn score-raw [chars]
  (count
    (filter
      #(pos? (java.util.Collections/indexOfSubList (apply list chars) (apply list (str " " % " "))))
      word-set)))

(defn score-ciphered [to-score cipher-key]
  (score-raw (cipher to-score cipher-key)))

(defn interesting-combos []
  (for [first-cipher (range (int \a) (inc (int \z)))
        second-cipher (range (int \a) (inc (int \z)))
        third-cipher (range (int \a) (inc (int \z)))]
    (let [the-cipher (map char [first-cipher second-cipher third-cipher])
          the-score (score-ciphered cipher-file the-cipher)]
      (if (< 8 the-score)
        (prn the-cipher the-score)))))
