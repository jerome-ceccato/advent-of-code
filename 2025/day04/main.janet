(defn insert-if-roll
  [data x y c]
  (if (= c (chr "@"))
    (put data [x y] true)))

(defn build-map
  [data y line]
  (def enumerated (map tuple (range 10000) line))
  (reduce (fn [acc (x c)] (do (insert-if-roll acc x y c) acc)) data enumerated))

(defn parse-input
  [&opt name]
  (default name "input")
  (->>
    name
    (slurp)
    (string/split "\n")
    (map tuple (range 10000))
    (reduce (fn [acc (y line)] (build-map acc y line)) @{})))

(def offsets @[[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])

(defn can-access?
  [data (x y)]
  (let [neighbors (map (fn [(xx yy)] [(+ x xx) (+ y yy)]) offsets)
        taken (count |(get data $0 false) neighbors)]
    (< taken 4)))

(defn p1
  [data]
  (count |(can-access? data $0) (keys data)))

(defn p2
  [acc data]
  (def accessed (filter |(can-access? data $0) (keys data)))
  (if (empty? accessed) acc
    (do
      (each k accessed (put data k nil))
      (p2 (+ acc (length accessed)) data))))

(->> (parse-input) (p1) (pp))
(->> (parse-input) (p2 0) (pp))
