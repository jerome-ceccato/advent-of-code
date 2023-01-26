#lang racket

(require threading)

(define tile-size 10)
(define image-size 3)
;(define image-size 12)

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
         ;all -> [[#(id, tile)]]
         [all-flat (map vector->list (flatten all))])
    ; all-flat -> [(id, tile)]
    ; filter out the ones that dont match the previous solved tiles (n-1 (left), n-3 (up))
    ; if no solved tiles, return all
    ;(printf "all-candidates ~a -> ~a~n" (length remaining) (length (filter (λ (tr) (tile-fits solved (cadr tr))) all-flat)))
    (filter (λ (tr) (tile-fits solved (cadr tr))) all-flat)))

(define (solve-next tilemap remaining solved solved-ids nextref)
  (let ([filtered-remaining (filter (curry (negate equal?) (car nextref)) remaining)]
        [next-solved (append solved (cdr nextref))]
        [next-solved-ids (append solved-ids (list (car nextref)))])
    ;(printf "adding ~a~n" (car nextref))
    (solve-rec tilemap filtered-remaining next-solved next-solved-ids)))

; Tries all possible tiles in each position (as long as they fit)
(define (solve-rec tilemap remaining solved solved-ids)
  (printf "solve-rec ~a (~a)~n" (length solved) (length remaining))
  ; (if (equal? solved (list p1)) (printf ">>> p1, ~a~n" remaining) #f)
  ; (if (equal? solved (list p1 p2)) (printf ">>> p1 p2~n") #f)
  ; (if (equal? solved (list p1 p2 p3)) (printf ">>> p1 p2 p3~n") #f)
  (if (empty? remaining)
      (list (map string->number solved-ids) solved)
      (for*/first ([tileref (all-candidates tilemap remaining solved)] ;(id, tile)
                   [recur (list (solve-next tilemap remaining solved solved-ids tileref))]
                   #:when recur)
        recur)))

(define (solve tilemap)
  (solve-rec
   tilemap
   (hash-keys tilemap)
   ;(list "1951" "2311" "3079" "2729" "1427" "2473" "2971" "1489" "1171")
   (list)
   (list)))

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

;;;;

(define (print-tile tile)
  (for-each (λ (line) (for-each (λ (c) (if c (display "#") (display "."))) (vector->list line)) (display "\n")) (vector->list tile)))

(define solution (~> "input" (read-input) (solve)))
(printf "part 1: ~a~n" (~> solution (car) (get-corners)))
(define image (~> solution (cadr) (map remove-borders _) (build-image) (list->vector)))
;(printf "part 2: ~a~n" (~> solution (cadr) (build-image)))
;(for-each (lambda (x) (print-tile x) (display "\n")) (cadr solution))
(print-tile image)
; (display (get-corners (list 3457 2857 1987 1553 1327 1531 2837 1723 3919 1871 2129 3709 3631 3371 2141 1303 3011 2243 1277 1933 1319 3373 3389 1373 2897 3343 1823 2153 1657
;                             3989 3571 3089 3701 2749 2221 1013 1783 3323 2039 3221 2659 2131 2969 1493 2887 3929 3673 3229 1367 3001 1579 1753 1619 1831 3931 3313 2909 1171 3767 2549 3209 1879 2677 1609 3331 3539 3583 1901 2113 3623 1747 3137 2503 1481 2953 3739 1973 1217 1031 1613 1439 3449 2029 2417 2593 1237 2389 2803 1861 2357 3163 1123 1489 2791 3607 3643 2963 2399 1361 3329 2699 1979 2267 3079 3253 1423 2707 1429 2621 1117 2633 1907 1249 2683 1699 1129 1103 3319 2833 2089 3191 3251 3217 2477 2081 3917 1997 1549 3119 3491 1741 3049 1093 2269 2789 1321 1193 2333 3907 2971 3347 3187 1789 2111)))
;(for-each (lambda (x) (print-tile x) (display "\n")) res)
;(printf "part 2: ~a~n" (~> "input" (read-input) ))

; (define test (read-input "input"))
; (define p1 (flip-v (hash-ref test "1951")))
; (define p2 (flip-v (hash-ref test "2311")))
; (define p3 (hash-ref test "3079"))
; (define p4 (flip-v (hash-ref test "2729")))
; (define p5 (flip-v (hash-ref test "1427")))
; (define p6 (rotate-once-clockwise (flip-h (hash-ref test "2473"))))
; (define p7 (flip-v (hash-ref test "2971")))

; (print-tile p1)
; (display "\n")
; (print-tile p2)
; (display "\n")
; (print-tile p3)
; (display "\n")
; (print-tile p4)
; (display "\n")
; (print-tile p5)
; (display "\n")
; (print-tile p6)
; (display "\n")
; (print-tile p7)

; (define next (all-candidates test (list "1427" "2473" "2971" "1489" "1171") (list p1 p2 p3 p4)))
; (printf "test: ~a~n" next)
; (for-each (lambda (x) (printf "~a:~n" (car x)) (print-tile (cadr x)) (display "\n")) next)