(ns euler.59)

(def word-set
  (into
    #{}
    (clojure.string/split
      (slurp "src/euler/word_list.txt")
      #"\r\n")))

(def cipher
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

(defn xor [a b] (if (= a b) 0 1))

(defn cipher [text cipher-key]
  (let [cipher-bits (mapcat bits-for-char cipher-key)
        repeated-cipher (mapcat identity (repeat cipher-bits))
        text-bits (mapcat bits-for-char text)
        ciphered-bits (map xor text-bits repeated-cipher)]
    (prn ciphered-bits)
    (map char-for-bits (partition 8 ciphered-bits))

    )


  )
