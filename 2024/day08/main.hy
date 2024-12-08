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

(defn add-if-in-bounds [antinodes bounds x y]
  (if (in-bounds? x y bounds)
    (do (.add antinodes #(x y)) True)
    False
  ))

(defn add-antinodes [antinodes bounds rec ax ay bx by]
  (let [dx (- ax bx)
        dy (- ay by)]
    (if rec 
      (do
        (setv i 0)
        (while (add-if-in-bounds antinodes bounds (+ ax (* dx i)) (+ ay (* dy i)))
          (setv i (+ i 1)))
        (setv i 0)
        (while (add-if-in-bounds antinodes bounds (- bx (* dx i)) (- by (* dy i)))
          (setv i (+ i 1))))
    (do
      (add-if-in-bounds antinodes bounds (+ ax dx) (+ ay dy))
      (add-if-in-bounds antinodes bounds (- bx dx) (- by dy))))))

(defn+ in-bounds? [x y [bound-x bound-y]]
  (and 
      (>= x 0)
      (>= y 0)
      (< x bound-x)
      (< y bound-y)))

(defn list-antinodes [points bounds rec]
  (let [points-sz (len points)
        antinodes #{}]
    (for [i (range points-sz)]
      (for [j (range (+ i 1) points-sz)]
        (let+ [[a ax ay] (get points i)
               [b bx by] (get points j)]
               (if (= a b)
                (add-antinodes antinodes bounds rec ax ay bx by)
                None))))
    antinodes))

(setv bounds (get-bounds input-data))
(setv points (list-points input-data))

(print (len (list-antinodes points bounds False)))
(print (len (list-antinodes points bounds True)))
