(defn parse-ranges
  [ranges]
  (map |(map scan-number (string/split "-" $0)) (string/split "\n" ranges)))

(defn parse-ids
  [ids]
  (map scan-number (string/split "\n" ids)))

(defn parse-input
  [&opt name]
  (default name "input")
  (let [data (slurp name)
        sections (string/split "\n\n" data)
        ranges (parse-ranges (first sections))
        ids (parse-ids (last sections))]
    {:ranges ranges :ids ids}))

(defn in-range?
  [a-range id]
  (and (>= id (first a-range)) (<= id (last a-range))))

(defn is-fresh?
  [ranges id]
  (not (nil? (find |(in-range? $0 id) ranges))))

(defn overlaps?
  [a b]
  (or (in-range? a (first b)) (in-range? a (last b)) (in-range? b (first a))))

(defn merge-ranges
  [ranges i j]
  (let [(l h) (get ranges i)
        (ll hh) (get ranges j)]
    (put ranges i @[(min l ll) (max h hh)])
    (array/remove ranges j)))

(defn simplify-once
  [ranges]
  (def len (length ranges))
  (var simplified false)
  (for i 0 len
    (for j (+ i 1) len
      (if simplified (break))
      (if (overlaps? (get ranges i) (get ranges j))
        (do (merge-ranges ranges i j) (set simplified true) (break)))))
  simplified)

(defn simplify-ranges
  [ranges]
  (while (simplify-once ranges))
  ranges)

(defn items-in-ranges
  [ranges]
  (sum (map (fn [(l h)] (+ (- h l) 1)) ranges)))

(def data (parse-input))
(->> (count |(is-fresh? (data :ranges) $0) (data :ids)) (pp))
(->> (simplify-ranges (data :ranges)) (items-in-ranges) (pp))
