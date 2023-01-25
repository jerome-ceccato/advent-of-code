#lang racket

(require threading)

(define tile-size 10)
(define image-size 3)

(define (vector-reverse v)
  (~> v (vector->list) (reverse) (list->vector)))

(define (parse-tiles lines)
  (~>>
   lines
   (map (λ~>> (string->list) (map (λ (c) (eqv? c #\#))) (list->vector)))
   (list->vector)))

(define (read-input filename)
  (for/hash ([tile (string-split (file->string filename) "\n\n")])
    (match-let ([(list-rest tilename lines) (string-split tile "\n")])
      (values (cadr (regexp-match #px"Tile (\\d+):" tilename)) (parse-tiles lines)))))

(define (flip-v tile)
  (vector-reverse tile))

(define (flip-h tile)
  (vector-map vector-reverse tile))

(define (rotate-once-clockwise tile)
  (for/vector ([x (in-range tile-size)])
    (vector-reverse (for/vector ([y (in-range tile-size)])
                      (vector-ref (vector-ref tile y) x)))))

(define (rotate tile)
  (let* ([r90 (rotate-once-clockwise tile)]
         [r180 (rotate-once-clockwise r90)]
         [r270 (rotate-once-clockwise r180)])
    (list tile r90 r180 r270)))

(define (all-orientations tile)
  (let* ([a (list tile (flip-v tile))]
         [b (append a (map flip-h a))])
    (flatten (map rotate b))))

(define (value-at tile x y)
  (if (vector-ref (vector-ref tile y) x) 1 0))

; Returns a list of numbers representing the NESW edges
(define (side-ids tile)
  (list
   (for/fold ([acc 0]) ([x (in-range tile-size)]) (+ (* 2 acc) (value-at tile x 0)))
   (for/fold ([acc 0]) ([x (in-range tile-size)]) (+ (* 2 acc) (value-at tile x (sub1 tile-size))))
   (for/fold ([acc 0]) ([y (in-range tile-size)]) (+ (* 2 acc) (value-at tile 0 y)))
   (for/fold ([acc 0]) ([y (in-range tile-size)]) (+ (* 2 acc) (value-at tile (sub1 tile-size) y)))))

; Returns a tuple of the left side and right side matching, or empty if no match
#;(define (try-connect lhs rhs)
    (for/first ([lid (side-ids lhs)]
                [lside (in-range 4)]
                #:when #t
                [rid (side-ids rhs)]
                [rside (in-range 4)]
                #:when (= lid rid))
      (list lside rside)))

; Tries to find any pair of connecting tiles to kickstart the process
#;(define (find-match tilemap)
    (for*/first ([lhs (hash-keys tilemap)]
                 [rhs (hash-keys tilemap)]
                 #:when (and (not (equal? lhs rhs)))
                 [lhs-o (all-orientations (hash-ref tilemap lhs))]
                 [rhs-o (all-orientations (hash-ref tilemap rhs))]
                 [connection (list (try-connect lhs-o rhs-o))]
                 #:when connection)
      (list (list lhs lhs-o) (list rhs rhs-o) connection)))

(define (fits-dir lhs rhs dir)
  (= (list-ref (side-ids lhs) dir) (list-ref (side-ids rhs) (modulo (+ dir 2) 4))))

(define (tile-fits solved candidate)
  (if (empty? solved) #t
      (and (fits-dir (last solved) candidate 1) ;
           (if (< (length solved) image-size) #t
               (fits-dir (list-ref solved (- (length solved) image-size)) candidate 2)))))

; Returns a list of tuples (id, rotated tile) that would fit in the next position
(define (all-candidates tilemap remaining solved)
  (let* ([all (map (λ (id) (map (λ (tile) (vector id tile)) (all-orientations (hash-ref tilemap id)))) remaining)]
         ;all -> [[#(id, tile)]]
         [all-flat (map vector->list (flatten all))])
    ; all-flat -> [(id, tile)]
    ; filter out the ones that dont match the previous solved tiles (n-1 (left), n-3 (up))
    ; if no solved tiles, return all
    ;(printf "all-candidates ~a -> ~a~n" (length all-flat) (length (filter (λ (tr) (tile-fits solved (cadr tr))) all-flat)))
    (filter (λ (tr) (tile-fits solved (cadr tr))) all-flat)))

(define (solve-next tilemap remaining solved nextref)
  (let ([filtered-remaining (filter (curry (negate equal?) (car nextref)) remaining)]
        [next-solved (cons (cadr nextref) solved)])
    (printf "solve-next ~a (~a)~n" (car nextref) (length solved))
    (solve-rec tilemap filtered-remaining next-solved)))

; Tries all possible tiles in each position (as long as they fit)
(define (solve-rec tilemap remaining solved)
  (if (empty? remaining)
      solved
      (for*/first ([tileref (all-candidates tilemap remaining solved)] ;(id, tile)
                   [recur (list (solve-next tilemap remaining solved tileref))]
                   #:when recur)
        recur)))

(define (solve tilemap)
  (solve-rec
   tilemap
   (hash-keys tilemap)
   ;(map (λ (_) #f) (hash-keys tilemap))
   (list)))
;;;;

(define (print-tile tile)
  (for-each (λ (line) (for-each (λ (c) (if c (display "#") (display "."))) (vector->list line)) (display "\n")) (vector->list tile)))

;(define test (hash-ref (read-input "input") "2311"))

(printf "part 1: ~a~n" (~> "input" (read-input) (solve)))
;(printf "part 2: ~a~n" (~> "input" (read-input) ))

;(printf "all ids ~a~n" (~>> "input" (read-input) (find-match)))
;(printf "test: ~a~n" (~> test (side-ids)))
;(printf "c: ~a~n" (try-connect (hash-ref (read-input "input") "1951") (hash-ref (read-input "input") "2311")))