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

(define (at seats width height coords)
  (match-let ([(vector y x) coords])
    (cond
      [(< x 0) 'oob]
      [(< y 0) 'oob]
      [(>= x width) 'oob]
      [(>= y height) 'oob]
      [else (vector-ref seats (+ x (* y width)))])))

(define (sight seats width height from offset)
  (let ([next (vector-map + from offset)])
    (if
     (eq? (at seats width height next) 'empty)
     (sight seats width height next offset)
     (at seats width height next))))

(define (nearby seats width height from offset ruleset)
  (let ([x (remainder from width)]
        [y (quotient from width)])
    (if (eq? ruleset 'adjacent)
        (~>>
         (vector-map + (vector y x) offset)
         (at seats width height))
        (sight seats width height (vector y x) offset))))

(define (around seats width height target ruleset)
  (map (λ~> (nearby seats width height target _ ruleset)) offsets))

(define (taken-seats-around seats width height target ruleset)
  (count (curry eq? 'taken) (around seats width height target ruleset)))

(define (taken-seats-tolerance ruleset)
  (if (eq? ruleset 'adjacent) 4 5))

(define (updated seats width height target ruleset)
  (match (vector-ref seats target)
    ['empty 'empty]
    ['available (if (>= (taken-seats-around seats width height target ruleset) 1) 'available 'taken)]
    ['taken (if (>= (taken-seats-around seats width height target ruleset) (taken-seats-tolerance ruleset)) 'available 'taken)]))

(define (run-once seats width height ruleset)
  (for/vector #:length (vector-length seats) ([coord (in-range (vector-length seats))])
    (updated seats width height coord ruleset)))

(define (run seats width height ruleset)
  (let ([next (run-once seats width height ruleset)])
    (if
     (equal? seats next)
     (vector-count (curry eq? 'taken) seats)
     (run next width height ruleset))))


(printf "part 1: ~a~n" (~> "input" (read-input) (flatten-input) (append (list 'adjacent)) (apply run _)))
(printf "part 2: ~a~n" (~> "input" (read-input) (flatten-input) (append (list 'line-of-sight)) (apply run _)))
