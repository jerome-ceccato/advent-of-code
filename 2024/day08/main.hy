(require 
  hyrule [-> ->>]
  hyrule.destructure [let+ fn+ defn+]
  hyrule.anaphoric * :readers [%])

(with [file (open "input" "r")]
  (setv input-data (-> 
    file
    .read
    (.split "\n"))))

(defn get-bounds [data]
  [(len data) (len (get data 0))])

(defn list-points [data]
  (let [points []]
    (for [y (range (len data))]
      (for [x (range (len (get data y)))]
        (let [c (get (get data y) x)]
          (if (!= "." c)
            (.append points #(c x y))
            None))))
    points))

(defn add-antinodes [antinodes ax ay bx by]
  (let [dx (- ax bx)
        dy (- ay by)]
    (.add antinodes #((+ ax dx) (+ ay dy)))
    (.add antinodes #((- bx dx) (- by dy)))))

(defn+ filter-inbounds [antinodes [bound-x bound-y]]
  (lfor [x y] antinodes 
    :if (and 
      (>= x 0)
      (>= y 0)
      (< x bound-x)
      (< y bound-y)) 
    #(x y)))

(defn list-antinodes [points]
  (let [points-sz (len points)
        antinodes #{}]
    (for [i (range points-sz)]
      (for [j (range (+ i 1) points-sz)]
        (let+ [[a ax ay] (get points i)
               [b bx by] (get points j)]
               (if (= a b)
                (add-antinodes antinodes ax ay bx by)
                None))))
    antinodes))

(-> input-data list-points list-antinodes (filter-inbounds (get-bounds input-data)) len print)

