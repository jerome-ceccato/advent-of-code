#lang racket

(require threading)

(define read-input 
  (λ~>>
    (file->lines)
    (map string->number)))

(define (find-entries n)
  (~>>
    (read-input "input")
    (combinations _ n)
    (findf (λ (lst) (= 2020 (foldl + 0 lst))))
    (foldl * 1)))


(printf "part 1: ~a~n" (find-entries 2))
(printf "part 2: ~a~n" (find-entries 3))
