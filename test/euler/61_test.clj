(ns euler.61-test
  (:use clojure.test
        euler.61
        euler.prime))

(deftest
  generators
  (is (= 1 (first naturals)))
  (is (= [1 3 6 10 15] (take 5 triangles)))
  (is (= [1 4 9 16 25] (take 5 squares)))
  (is (= [1 5 12 22 35] (take 5 pentagons)))
  (is (= [1 6 15 28 45] (take 5 hexagons)))
  (is (= [1 7 18 34 55] (take 5 heptagons)))
  (is (= [1 8 21 40 65] (take 5 octagons))))

(deftest
  filtering-for-four-digits
  (is (= 1000 (apply min (pick-four-digit-numbers (range 900 1100)))))
  (is (= 9999 (apply max (pick-four-digit-numbers (range 9900 11111)))))
  (is (<= (apply max triangles-set) 9999))
  (is (>= (apply min triangles-set) 1000))

  )

(deftest cyclic-ness
         (is (cyclic? 8128 2882)))

(deftest addability
         (is (addable? [8128] 2882 3))
         (is (addable? [8128 2882] 8281 3))
         (is (not (addable? [8128 2882] 8282 3))))

(deftest example-from-problem
         (is (= [8128 2882 8281] (:numbers-picked (find-it 3 #{triangles-set squares-set pentagons-set}))))
         (prn
           (find-it 6 #{triangles-set
                        squares-set
                        pentagons-set
                        hexagons-set
                        heptagons-set
                        octagons-set})))

