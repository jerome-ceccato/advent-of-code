(def grammar
  ~{:left (sequence (replace "L" :left) (number :d+ 10))
    :right (sequence (replace "R" :right) (number :d+ 10))
    :main (choice :left :right)})

(defn transform-data
  [parsed]
  (if (= (first parsed) :left)
    (- (last parsed))
    (last parsed)))

(defn parse-input
  [&opt name]
  (default name "input")
  (->>
    name
    (slurp)
    (string/split "\n")
    (map |(->>
            $0
            (peg/match grammar)
            (transform-data)))))

(defn rotate
  [acc new]
  (% (+ (% (+ acc new) 100) 100) 100))

(defn traverse-rec
  [current acc items]
  (if (empty? items)
    acc
    (let [[x & xs] items
          skips (div (math/abs x) 100)
          step (% x 100)
          nxt (+ current step)
          passed (if (and (not= step 0) (not= current 0) (or (<= nxt 0) (>= nxt 100))) 1 0)]
      (traverse-rec (rotate nxt 0) (+ acc skips passed) xs))))

(defn p1
  [data]
  (->> data (accumulate rotate 50) (count zero?)))

(defn p2
  [data]
  (->> data (traverse-rec 50 0)))

(->> (parse-input) (p1) (pp))
(->> (parse-input) (p2) (pp))
