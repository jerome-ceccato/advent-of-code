#lang racket

(require threading)

(define input (file->lines "input"))

(define (is-tree? trees pos)
  (~>
   trees
   (list-ref (vector-ref pos 1))
   (string-ref (vector-ref pos 0))
   (eqv? #\#)))

(define (count-trees trees slope [current #(0 0)])
  (let* ([height (length trees)]
         [width (string-length (car trees))]
         [x (modulo (+ (vector-ref slope 0) (vector-ref current 0)) width)]
         [y (+ (vector-ref slope 1) (vector-ref current 1))]
         [next (vector x y)])
    (if (>= y height)
        0
        (+ (if (is-tree? trees next) 1 0) (count-trees trees slope next)))))

(define (test-slopes trees)
  (~>>
   (list #(1 1) #(3 1) #(5 1) #(7 1) #(1 2))
   (map (λ~>> (count-trees trees)))
   (foldl * 1)))


(printf "part 1: ~a~n" (count-trees input #(3 1)))
(printf "part 2: ~a~n" (test-slopes input))