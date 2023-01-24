#lang racket

(require threading)

(define (read-input filename)
  (for/set ([line (file->lines filename)]
            [y (in-naturals)]
            #:when #t
            [c (string->list line)]
            [x (in-naturals)]
            #:when (eqv? c #\#))
    (list x y 0 0)))

(define (neighbors-w w dim)
  (if (= 3 dim) (list w) (in-range (sub1 w) (+ 2 w))))

(define (neighbors pos dim)
  (match-let ([(list x y z w) pos])
    (for*/set ([xa (in-range (sub1 x) (+ 2 x))]
               [ya (in-range (sub1 y) (+ 2 y))]
               [za (in-range (sub1 z) (+ 2 z))]
               [wa (neighbors-w w dim)]
               #:when (not (equal? pos (list xa ya za wa))))
      (list xa ya za wa))))

(define (count-on-neighbors board pos dim)
  (for/sum ([npos (neighbors pos dim)]
            #:when (set-member? board npos))
    1))

(define (pos-to-consider board dim)
  (for/fold ([positions (set)])
            ([current board])
    (set-union positions (neighbors current dim))))

(define (should-be-on board pos dim)
  (if
   (set-member? board pos)
   (<= 2 (count-on-neighbors board pos dim) 3)
   (= 3 (count-on-neighbors board pos dim))))

(define (simulate-cycle board dim)
  (for/set ([pos (pos-to-consider board dim)]
            #:when (should-be-on board pos dim))
    pos))

(define (count-cells-after-cycles board cycles dim)
  (~>
   board
   (foldl (λ (_ b) (simulate-cycle b dim)) _ (stream->list (in-range cycles)))
   (set-count)))


(printf "part 1: ~a~n" (~> "input" (read-input) (count-cells-after-cycles 6 3)))
(printf "part 2: ~a~n" (~> "input" (read-input) (count-cells-after-cycles 6 4)))
