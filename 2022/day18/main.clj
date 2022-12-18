(require '[clojure.string :as str])

(defrecord Point [x y z])
(defrecord Bounds [low high])

;; Point helpers
(defn point-apply
  ([f a b]
   (->Point (f (:x a) (:x b)) (f (:y a) (:y b)) (f (:z a) (:z b))))
  ([f a]
   (->Point (f (:x a)) (f (:y a)) (f (:z a)))))

(defn point-cmp [f a b]
  (list (f (:x a) (:x b)) (f (:y a) (:y b)) (f (:z a) (:z b))))

;; Bounds helpers
(defn get-bounds [points]
  (->Bounds
   (point-apply #(- % 1) (reduce #(point-apply min %1 %2) points))
   (point-apply #(+ % 1) (reduce #(point-apply max %1 %2) points))))

(defn in-bounds? [p bounds]
  (and
   (reduce #(and %1 %2) (point-cmp >= p (:low bounds)))
   (reduce #(and %1 %2) (point-cmp <= p (:high bounds)))))

;; Input
(defn parse-input [file] 
  (->>
   (slurp file)
   (str/split-lines)
   (map #(str/split % #","))
   (map #(map (fn [x] (Integer/parseInt x)) %))
   (map #(apply ->Point %)) 
   (set)))

;; Exploring cubes
(defn adjacent [p]
  (list 
   (->Point (+ (:x p) 1) (:y p) (:z p))
   (->Point (- (:x p) 1) (:y p) (:z p))
   (->Point (:x p) (+ (:y p) 1) (:z p))
   (->Point (:x p) (- (:y p) 1) (:z p))
   (->Point (:x p) (:y p) (+ (:z p) 1))
   (->Point (:x p) (:y p) (- (:z p) 1))))

(defn count-sides [p f]
  (->>
   (adjacent p) 
   (map #(if (f %) 1 0))
   (reduce +)))

(defn count-all [points f]
  (->>
   (map #(count-sides % f) points)
   (reduce +)))

(defn expansion-candidates [this points visited bounds]
  (->>
   (adjacent this)
   (filter #(in-bounds? % bounds))
   (filter #(not (contains? visited %)))
   (filter #(not (contains? points %)))))

(defn expand-steam [current points visited bounds] 
  (if (empty? current)
    visited
    (let [[this & other] current
          next (expansion-candidates this points visited bounds)]
      (recur (into other next) points (into visited next) bounds)))) 

;; Solution
(defn part1 [points]
  (count-all points #(not (contains? points %))))

(defn part2 [points]
  (let [bounds (get-bounds points)
        steam (expand-steam [(:low bounds)] points #{(:low bounds)} bounds)]
    (count-all points #(contains? steam %))))

;; Main
(let [input (parse-input "input")]
  (println (part1 input))
  (println (part2 input)))
