(def grammar
  (peg/compile ~{:range (group (sequence (number :d+ 10) "-" (number :d+ 10)))
                 :main (some (sequence :range (? ",")))}))

(defn parse-input
  [&opt name]
  (default name "input")
  (peg/match grammar (slurp name)))

(defn expand
  [arr]
  (array/push (range ;arr) (last arr)))

(defn is-valid?
  [n]
  (def s (string n))
  (or
    (= (% (length s) 2) 1)
    (not= ;(partition (/ (length s) 2) s))))

(defn is-invalid?
  [n]
  (def s (string n))
  (and
    (= (% (length s) 2) 0)
    (= ;(partition (/ (length s) 2) s))))

(defn is-invalid2?
  [n]
  (def s (string n))
  (and (> (length s) 1)
       (not (nil? (find |(= ;(partition $0 s)) (range 1 (+ 1 (/ (length s) 2))))))))

(def expanded-data (->> (parse-input) (map expand) (flatten)))
(->> expanded-data (filter is-invalid?) (sum) (pp))
(->> expanded-data (filter is-invalid2?) (sum) (pp))
