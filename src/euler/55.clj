(ns euler.55
  (:require [clojure.string :as string]))

(declare )

(defn kill-leading-zeros [string]
  (if (= "0" (str (first string)))
    (recur (rest string))
    string))

(defn parse-int-with-leading-zeros [number-string]
  (read-string (apply str (kill-leading-zeros number-string))))

(defn plus-its-reverse [number]
  (let [string-number (str number)
        reversed (string/reverse string-number)]
    (+' number
       (parse-int-with-leading-zeros reversed))))
        
(defn is-palindrome? [number]
  (let [string-number (str number)
        reversed (string/reverse string-number)]
    (= string-number reversed)))

(defn causes-palindrome? [number]
  (is-palindrome? (plus-its-reverse number)))

(defn lychrel-50?
  ([number]
  (lychrel-50? number 50))
  ([number remaining-iterations]
   (if (pos? remaining-iterations)
     (if (causes-palindrome? number)
       false
       (recur (plus-its-reverse number) (dec remaining-iterations)))
     true)))


(defn -main []
  (reduce 
    (fn [total current-number]
      (if (lychrel-50? current-number)
        (inc total)
        total))
      0
      (range 1 10000)))
