(defn parse-input
  [&opt name]
  (default name "input")
  (let [data (slurp name)
        numbers-raw (string/split "\n" data)
        ops-raw (array/pop numbers-raw)
        ops (->> ops-raw (string/split " ") (filter |(not (empty? $0))))]
    {:numbers numbers-raw :ops ops}))

(defn updating-numbers
  [f {:numbers numbers :ops ops}]
  {:numbers (f numbers) :ops ops})

(defn parse-numbers
  [numbers]
  (->>
    numbers
    (map |(->> $0 (string/split " ") (filter |(not (empty? $0)))))
    (map |(map scan-number $0))))

(defn do-op-regular
  [{:numbers numbers :ops ops} i]
  (def op (if (= (get ops i) "*") * +))
  (op ;(map |(get $0 i) numbers)))

(defn do-op-transposed
  [{:numbers numbers :ops ops} i]
  (def op (if (= (get ops i) "*") * +))
  (op ;(get numbers i)))

(defn do-ops
  [op-fct data]
  (def ops (data :ops))
  (map |(op-fct data $0) (range (length ops))))

(defn transpose
  [arr2d]
  (let [w (length arr2d)
        h (length (first arr2d))]
    (map (fn [y] (map (fn [x] (get (get arr2d x) y)) (range w))) (range h))))

(defn reorder-numbers
  [numbers]
  (->>
    numbers
    (transpose)
    (map |(string/from-bytes ;$0))
    (map string/trim)
    (partition-by empty?)
    (map |(filter |(not (empty? $0)) $0))
    (filter |(not (empty? $0)))
    (map |(map scan-number $0))))

(->> (parse-input) (updating-numbers parse-numbers) (do-ops do-op-regular) (sum) (pp))
(->> (parse-input) (updating-numbers reorder-numbers) (do-ops do-op-transposed) (sum) (pp))
