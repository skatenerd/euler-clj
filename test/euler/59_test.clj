(ns euler.59-test
  (:use clojure.test
        euler.59))

(deftest bits
   (testing "unbitify rebitify"
        (is (= 42 (unbitify (bitify 42)))))
   (testing "num char i"
        (is (= "R" (char-for-i 82))))
   (testing "num char bits"
        (is (= "R" (char-for-bits (bitify 82)))))
   (testing "num char bits"
        (is (= (bitify 82) (bits-for-char \R))))

  (testing "cipher a sequence of chars"
        (let [cipher-key [\A \A \A]
              to-cipher [\K \K \K \K \K \K]]
          (should= \* (first (cipher to-cipher cipher)))))
         )
