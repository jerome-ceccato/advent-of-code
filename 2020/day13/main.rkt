#lang racket

(require threading math/number-theory)

(define (read-input filename)
  (match-let ([(list ts buses) (file->lines filename)])
    (list
     (string->number ts)
     (~>>
      buses
      (string-split _ ",")
      (filter (curry (negate equal?) "x"))
      (map string->number)))))

(define (read-schedule filename)
  (let ([buses (~>> filename (file->lines) (cadr) (string-split _ ","))])
    (for/list ([bus buses]
               [i (in-naturals)]
               #:when (not (equal? bus "x")))
      (list i (string->number bus)))))

(define (bus-departs-at ts bus)
  (= 0 (modulo ts bus)))

(define (find-earliest ts buses)
  (let ([valid-bus (findf (curry bus-departs-at ts) buses)])
    (if valid-bus
        (list ts valid-bus)
        (find-earliest (add1 ts) buses))))

(define (find-depature data)
  (match-let* ([(list ts buses) data]
               [(list final-ts bus) (find-earliest ts buses)])
    (* (- final-ts ts) bus)))

(define (bus-remainder data)
  (match-let ([(list offset bus) data])
    (modulo (- bus offset) bus)))

(define (find-golden-ts data)
  ; this whole part2 solution is built-in
  (solve-chinese
   (map bus-remainder data)
   (map cadr data)))


(printf "part 1: ~a~n" (~> "input" (read-input) (find-depature)))
(printf "part 2: ~a~n" (~> "input" (read-schedule) (find-golden-ts)))
