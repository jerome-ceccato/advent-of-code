(defn split-line
  [line]
  (map |(- $0 (chr "0")) line))

(defn parse-input
  [&opt name]
  (default name "input")
  (->>
    name
    (slurp)
    (string/split "\n")
    (map split-line)))

(defn find-max
  [arr]
  (let [max-first-digit (max-of (array/slice arr 0 -2))
        idx (find-index |(= max-first-digit $0) arr)
        max-second-digit (max-of (array/slice arr (+ idx 1)))]
    (+ (* max-first-digit 10) max-second-digit)))

(defn find-max-rec
  [n acc arr]
  (if (= n 0) acc
    (let [max-first-digit (max-of (array/slice arr 0 (- n)))
          idx (find-index |(= max-first-digit $0) arr)
          rest (array/slice arr (+ idx 1))]
      (find-max-rec (- n 1) (+ (* acc 10) max-first-digit) rest))))

(->> (parse-input) (map find-max) (sum) (pp))
(->> (parse-input) (map |(find-max-rec 12 0 $0)) (sum) (pp))
