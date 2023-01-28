#lang racket

(require threading)

(define (parse-path line)
  (cond
    [(string-prefix? line "e") (cons 'e (parse-path (substring line 1)))]
    [(string-prefix? line "w") (cons 'w (parse-path (substring line 1)))]
    [(string-prefix? line "se") (cons 'se (parse-path (substring line 2)))]
    [(string-prefix? line "ne") (cons 'ne (parse-path (substring line 2)))]
    [(string-prefix? line "sw") (cons 'sw (parse-path (substring line 2)))]
    [(string-prefix? line "nw") (cons 'nw (parse-path (substring line 2)))]
    [else (list)]))

(define read-input
  (λ~>> (file->lines) (map parse-path)))

; taken from the legendary https://www.redblobgames.com/grids/hexagons
; (list q r s)
(define (cube-offset dir)
  (match dir
    ['e '(1 0 -1)]
    ['se '(0 1 -1)]
    ['sw '(-1 1 0)]
    ['w '(-1 0 1)]
    ['nw '(0 -1 1)]
    ['ne '(1 -1 0)]))

(define (cube-coords-from-path path)
  (for/fold ([coord '(0 0 0)])
            ([dir path])
    (map + coord (cube-offset dir))))

(define (initial-tiles paths)
  (let ([tiles (mutable-set)])
    (for ([path paths])
      (let ([coord (cube-coords-from-path path)])
        (if (set-member? tiles coord)
            (set-remove! tiles coord)
            (set-add! tiles coord))))
    tiles))

(define (neighbors tile)
  (~>>
   '(e se sw w nw ne)
   (map cube-offset)
   (map (λ~>> (map + tile)))))

(define (all-relevant-tiles on-tiles)
  (for*/set ([on on-tiles]
             [candidate (cons on (neighbors on))])
    candidate))

(define (tile-should-be-on? tile on-tiles)
  (let ([on-tiles-around (count (λ~>> (set-member? on-tiles)) (neighbors tile))])
    (if (set-member? on-tiles tile)
        (or (= 1 on-tiles-around) (= 2 on-tiles-around))
        (= 2 on-tiles-around))))

(define (flip-once tiles)
  (for/set ([candidate (all-relevant-tiles tiles)]
            #:when (tile-should-be-on? candidate tiles))
    candidate))

(define (flip-tiles og-tiles days)
  (for/fold ([tiles og-tiles])
            ([_ (in-range days)])
    (flip-once tiles)))


(printf "part 1: ~a~n" (~> "input" (read-input) (initial-tiles) (set-count)))
(printf "part 2: ~a~n" (~> "input" (read-input) (initial-tiles) (flip-tiles 100) (set-count)))
