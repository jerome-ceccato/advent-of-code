#lang racket

(require threading)

(define (parse-state c)
  (match c
    [#\. 'empty]
    [#\L 'available]
    [#\# 'taken]))

(define read-input
  (λ~>>
   (file->lines)
   (map (λ~>> (string->list) (map parse-state)))))

(define (flatten-input seats)
  (list (list->vector (flatten seats)) (length (first seats)) (length seats)))

(define offsets
  (list
   #(-1 -1)
   #(-1 0)
   #(-1 1)
   #(0 -1)
   #(0 1)
   #(1 -1)
   #(1 0)
   #(1 1)))

(define (around seats width height target)
  (let ([x (remainder target width)]
        [y (quotient target width)])
    (~>>
     offsets
     (map (λ~>> (vector-map + (vector y x))))
     (map (λ~>> (at seats width height))))))

(define (at seats width height coords)
  (match-let ([(vector y x) coords])
    (cond
      [(< x 0) 'oob]
      [(< y 0) 'oob]
      [(>= x width) 'oob]
      [(>= y height) 'oob]
      [else (vector-ref seats (+ x (* y width)))])))

(define (taken-seats-around seats width height target)
  (count (curry eq? 'taken) (around seats width height target)))

(define (updated seats width height target)
  (match (vector-ref seats target)
    ['empty 'empty]
    ['available (if (>= (taken-seats-around seats width height target) 1) 'available 'taken)]
    ['taken (if (>= (taken-seats-around seats width height target) 4) 'available 'taken)]))

(define (run-once seats width height)
  (for/vector #:length (vector-length seats) ([coord (in-range (vector-length seats))])
    (updated seats width height coord)))

(define (run seats width height)
  (let ([next (run-once seats width height)])
    (if
     (equal? seats next)
     (vector-count (curry eq? 'taken) seats)
     (run next width height))))


(printf "part 1: ~a~n" (~> "input" (read-input) (flatten-input) (apply run _)))
#;(printf "part 2: ~a~n" (~> "input" (read-input) ))
