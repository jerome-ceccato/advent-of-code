#lang racket

(require threading)

(define (parse-line line)
  (list
   (substring line 0 1)
   (string->number (substring line 1))))

(define read-input
  (λ~>>
   (file->lines)
   (map parse-line)))

(define (encode-dir dir)
  (match dir
    ["N" 0]
    ["E" 1]
    ["S" 2]
    ["W" 3]))

(define (turn dir a)
  (remainder (+ dir (quotient a 90) 4) 4))

(define (offset-for-dir dir)
  (match dir
    [0 '(-1 0)]
    [1 '(0 1)]
    [2 '(1 0)]
    [3 '(0 -1)]))

(define distance
  (λ~>>
   (car)
   (map abs)
   (foldl + 0)))

(define (move-entity pos dir value)
  (~>>
   dir
   (offset-for-dir)
   (map (curry * value))
   (map + pos)))

(define (move-once i ship)
  (match-let ([(list pos dir) ship]
              [(list action value) i])
    (match action
      [(or "N" "E" "S" "W") (list (move-entity pos (encode-dir action) value) dir)]
      ["F" (list (move-entity pos dir value) dir)]
      ["L" (list pos (turn dir (- value)))]
      ["R" (list pos (turn dir value))])))

(define (run instructions [ship (list '(0 0) (encode-dir "E"))])
  (match instructions
    [(list-rest i xs) (run xs (move-once i ship))]
    [_ ship]))

(define (rotate-waypoint waypoint a)
  (let*
      ([rad (degrees->radians a)]
       [cosa (cos rad)]
       [sina (sin rad)]
       [y (first waypoint)]
       [x (second waypoint)])
    (list
     (round ((y . * . cosa) . + . (x . * . sina)))
     (round ((x . * . cosa) . - . (y . * . sina))))))

(define (move-towards-waypoint pos waypoint value)
  (map + pos (map (curry * value) waypoint)))

(define (move-waypoint-once i data)
  (match-let ([(list pos waypoint) data]
              [(list action value) i])
    (match action
      [(or "N" "E" "S" "W") (list pos (move-entity waypoint (encode-dir action) value))]
      ["F" (list (move-towards-waypoint pos waypoint value) waypoint)]
      ["L" (list pos (rotate-waypoint waypoint (- value)))]
      ["R" (list pos (rotate-waypoint waypoint value))])))

(define (run-waypoint instructions [data (list '(0 0) '(-1 10))])
  (match instructions
    [(list-rest i xs) (run-waypoint xs (move-waypoint-once i data))]
    [_ data]))


(printf "part 1: ~a~n" (~> "input" (read-input) (run) (distance)))
(printf "part 2: ~a~n" (~> "input" (read-input) (run-waypoint) (distance)))
