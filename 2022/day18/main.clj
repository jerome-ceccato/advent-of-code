(require '[clojure.string :as str])

(defrecord Point [x y z])

(defn parse-input [file] 
  (->>
   (slurp file)
   (str/split-lines)
   (map #(str/split % #","))
   (map #(map (fn [x] (Integer/parseInt x)) %))
   (map #(apply ->Point %)) 
   (set)))

(defn adjacent [p]
  (list 
   (->Point (+ (:x p) 1) (:y p) (:z p))
   (->Point (- (:x p) 1) (:y p) (:z p))
   (->Point (:x p) (+ (:y p) 1) (:z p))
   (->Point (:x p) (- (:y p) 1) (:z p))
   (->Point (:x p) (:y p) (+ (:z p) 1))
   (->Point (:x p) (:y p) (- (:z p) 1))))

(defn open-sides [points p]
  (->>
   (adjacent p)
   (map #(contains? points %))
   (map #(if % 0 1))
   (reduce +)))

(defn part1 [points]
  (->>
   (map #(open-sides points %) points)
   (reduce +)))

(->>
 (parse-input "input")
 (part1)
 (print))
