(ns euler.61
  (:use euler.prime
        euler.dfs
        ))

(def naturals (iterate inc 1))

(defn map-naturals [generator]
  (map generator naturals))

(defn pick-four-digit-numbers [numbers]
  (set (take-while #(<= % 9999) (drop-while #(< % 1000) numbers))))

;Triangle    P3,n=n(n+1)/2   1, 3, 6, 10, 15, ...
;Square    P4,n=n2   1, 4, 9, 16, 25, ...
;Pentagonal    P5,n=n(3n−1)/2    1, 5, 12, 22, 35, ...
;Hexagonal   P6,n=n(2n−1)    1, 6, 15, 28, 45, ...
;Heptagonal    P7,n=n(5n−3)/2    1, 7, 18, 34, 55, ...
;Octagonal   P8,n=n(3n−2)    1, 8, 21, 40, 65, ...
(def triangles (map-naturals #(/ (* % (inc %)) 2)))
(def squares (map-naturals #(* % %)))
(def pentagons (map-naturals #(/ (* % (dec (* 3 %))) 2)))
(def hexagons (map-naturals #(* % (dec (* % 2)))))
(def heptagons (map-naturals #(/ (* % (- (* 5 %) 3)) 2)))
(def octagons (map-naturals #(* % (- (* 3 %) 2))))

(def triangles-set (pick-four-digit-numbers triangles))
(def squares-set (pick-four-digit-numbers squares))
(def pentagons-set (pick-four-digit-numbers pentagons))
(def hexagons-set (pick-four-digit-numbers hexagons))
(def heptagons-set (pick-four-digit-numbers heptagons))
(def octagons-set (pick-four-digit-numbers octagons))

(defn- first-two-digits [number]
  (int (/ number 100)))

(defn- last-two-digits [number]
  (mod number 100))

(defn cyclic? [n1 n2]
  (= (last-two-digits n1) (first-two-digits n2)))

(defn addable? [so-far to-add max-length]
  (cond
    (empty? so-far)
    true
    (= (dec max-length) (count so-far))
    (and
      (cyclic? to-add (first so-far))
      (cyclic? (last so-far) to-add))
    :else
    (cyclic? (last so-far) to-add)))

(defn nodes-for-figurate [figurate numbers-so-far remaining-figurates max-length]
  (map
    (fn [to-add] {:numbers-picked (conj numbers-so-far to-add)
                  :remaining-figurates (disj remaining-figurates figurate)})
    (filter
      #(addable? numbers-so-far % max-length)
      figurate)))

(defn with-addable-numbers [node max-length]
  (prn (:numbers-picked node))
  (let [numbers-so-far (:numbers-picked node)
        remaining-figurates (:remaining-figurates node)]
    (apply
      concat
      (map #(nodes-for-figurate % numbers-so-far remaining-figurates max-length) remaining-figurates))))


(defn find-it [max-length figurates]
  (dfs
    {:remaining-figurates figurates :numbers-picked [] }
    #(with-addable-numbers % max-length)
    #(= max-length (count (:numbers-picked %)))))
