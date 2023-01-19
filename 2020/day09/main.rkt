#lang racket

(require threading)

(define window-size 25)

(define read-input
  (λ~>>
   (file->lines)
   (map string->number)))

(define (slice lst i)
  (take (drop lst (- i window-size)) window-size))

(define (has-sum n prev)
  (for*/first ([x prev]
               [y prev]
               #:when (and (not (= x y))
                           (= n (+ x y))))
    #t))

(define (find-invalid numbers)
  (for/first ([i (in-range (length numbers))]
              #:when (and (>= i window-size)
                          (not (has-sum (list-ref numbers i) (slice numbers i)))))
    (list-ref numbers i)))

(define (sublist-sum-to-target numbers target)
  (for*/first ([i (in-range (length numbers))]
               [sum (list (foldl + 0 (take numbers (add1 i))))]
               #:when (= target sum)
               #:break (> target sum))
    (take numbers (add1 i))))

(define (find-weak-range numbers)
  (let ([target (find-invalid numbers)])
    (for*/first ([i (in-range (length numbers))]
                 [res (list (sublist-sum-to-target (drop numbers i) target))]
                 #:when res)
      res)))

(define (find-weakness numbers)
  (let ([n-range (find-weak-range numbers)])
    (+ (apply min n-range) (apply max n-range))))


(printf "part 1: ~a~n" (~> "input" (read-input) (find-invalid)))
(printf "part 2: ~a~n" (~> "input" (read-input) (find-weakness)))
