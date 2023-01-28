#lang racket

(require threading)

(define read-input
  (λ~>> (file->lines) (map string->number)))

(define (next n subject)
  (modulo (* n subject) 20201227))

(define (find-loop-size target)
  (for/fold ([n 1] [loop 0] #:result loop)
            ([_ (in-naturals)] #:break (= n target))
    (values (next n 7) (add1 loop))))

(define (get-key from loop-size)
  (for/fold ([n 1])
            ([_ (in-range loop-size)])
    (next n from)))

(define (find-encryption-key input)
  (get-key (car input) (find-loop-size (cadr input))))


(printf "part 1: ~a~n" (~> "input" (read-input) (find-encryption-key)))
