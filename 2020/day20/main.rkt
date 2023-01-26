#lang racket

(require threading)

(define tile-size 10)
(define image-size 12)

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
  (for/vector ([x (in-range (vector-length tile))])
    (vector-reverse (for/vector ([y (in-range (vector-length (vector-ref tile 0)))])
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
   (for/fold ([acc 0]) ([x (in-range tile-size)]) (+ (* 2 acc) (value-at tile x 0))) ;N
   (for/fold ([acc 0]) ([y (in-range tile-size)]) (+ (* 2 acc) (value-at tile (sub1 tile-size) y))) ;E
   (for/fold ([acc 0]) ([x (in-range tile-size)]) (+ (* 2 acc) (value-at tile x (sub1 tile-size)))) ;S
   (for/fold ([acc 0]) ([y (in-range tile-size)]) (+ (* 2 acc) (value-at tile 0 y))))) ;W

(define (fits-dir lhs rhs dir)
  (= (list-ref (side-ids lhs) dir) (list-ref (side-ids rhs) (modulo (+ dir 2) 4))))

; Check that the tile fits with the left and top tiles already in place (if applicable)
(define (tile-fits solved candidate)
  (if (empty? solved) #t
      (and (if (= (modulo (length solved) image-size) 0) #t (fits-dir (last solved) candidate 1))
           (if (< (length solved) image-size) #t
               (fits-dir (list-ref solved (- (length solved) image-size)) candidate 2)))))

; Returns a list of tuples (id, rotated tile) that would fit in the next position
(define (all-candidates tilemap remaining solved)
  (let* ([all (map (λ (id) (map (λ (tile) (vector id tile)) (all-orientations (hash-ref tilemap id)))) remaining)]
         [all-flat (map vector->list (flatten all))])
    (filter (λ (tr) (tile-fits solved (cadr tr))) all-flat)))

(define (solve-next tilemap remaining solved solved-ids nextref)
  (let ([filtered-remaining (filter (curry (negate equal?) (car nextref)) remaining)]
        [next-solved (append solved (cdr nextref))]
        [next-solved-ids (append solved-ids (list (car nextref)))])
    (solve tilemap filtered-remaining next-solved next-solved-ids)))

; Tries all possible tiles in each position (as long as they fit)
(define (solve tilemap [remaining (hash-keys tilemap)] [solved (list)] [solved-ids (list)])
  (if (empty? remaining)
      (list (map string->number solved-ids) solved)
      (for*/first ([tileref (all-candidates tilemap remaining solved)] ;(id, tile)
                   [recur (list (solve-next tilemap remaining solved solved-ids tileref))]
                   #:when recur)
        recur)))

(define (get-corners result)
  (*
   (list-ref result 0)
   (list-ref result (sub1 image-size))
   (list-ref result (* image-size (sub1 image-size)))
   (list-ref result (sub1 (* image-size image-size)))))

(define (remove-borders tile)
  (~>>
   tile
   (vector-copy _ 1 (sub1 (vector-length tile)))
   (vector-map (λ (line) (vector-copy line 1 (sub1 (vector-length line)))))))

(define (build-image result)
  (flatten
   (for/list ([y (in-range image-size)])
     (for/list ([line (in-range (- tile-size 2))])
       (apply vector-append
              (for/list ([x (in-range image-size)])
                (vector-ref (list-ref result (+ x (* y image-size))) line)))))))

; empty spaces are #t, monster is #f, so we can bitwise-or to test the whole pattern
; and bitwise-and to remove the sea monster afterwards
(define sea-monster-mask
  #(#(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t)
    #(#f #t #t #t #t #f #f #t #t #t #t #f #f #t #t #t #t #f #f #f)
    #(#t #f #t #t #f #t #t #f #t #t #f #t #t #f #t #t #f #t #t #t)))

(define (image-count image)
  (for*/sum ([line image]
             [pixel line]
             #:when pixel)
    1))

(define (remove-sea-monster! image ix iy)
  (for* ([y (in-range (vector-length sea-monster-mask))]
         [x (in-range (vector-length (vector-ref sea-monster-mask 0)))])
    (vector-set! (vector-ref image (+ iy y)) (+ ix x)
                 (and (vector-ref (vector-ref sea-monster-mask y) x)
                      (vector-ref (vector-ref image (+ iy y)) (+ ix x)))))
  image)

(define (sea-monster-at? image ix iy)
  (for*/and ([y (in-range (vector-length sea-monster-mask))]
             [x (in-range (vector-length (vector-ref sea-monster-mask 0)))])
    (or (vector-ref (vector-ref sea-monster-mask y) x)
        (vector-ref (vector-ref image (+ iy y)) (+ ix x)))))

(define (remove-sea-monsters image)
  (let ([mut-image (vector-map vector-copy image)])
    (for* ([y (in-range (- (vector-length image) (vector-length sea-monster-mask)))]
           [x (in-range (- (vector-length (vector-ref image y)) (vector-length (vector-ref sea-monster-mask 0))))]
           #:when (sea-monster-at? mut-image x y))
      (remove-sea-monster! mut-image x y))
    mut-image))

(define (size-without-sea-monsters og-image)
  (for*/first ([image (all-orientations og-image)]
               [result (list (remove-sea-monsters image))]
               #:when (not (= (image-count image) (image-count result))))
    (image-count result)))


(define solution (~> "input" (read-input) (solve)))
(printf "part 1: ~a~n" (~> solution (car) (get-corners)))
(define image (~> solution (cadr) (map remove-borders _) (build-image) (list->vector)))
(printf "part 2: ~a~n" (~> image (size-without-sea-monsters)))
