#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->lines)
   (map string->number)
   (sort _ <)))

(define (steps numbers)
  (let ([inner-steps (for/list ([a numbers]
                                [b (cdr numbers)])
                       (- b a))])
    (append (list (car numbers)) inner-steps '(3))))

(define (count-diffs steps)
  (* (count (curry = 1) steps) (count (curry = 3) steps)))

(define (slice-next numbers)
  (match numbers
    [(list-rest n xs) (filter (curry >= (+ n 3)) xs)]
    [_ '()]))

(define (valid-steps numbers [depth 0] [memo (make-hash)])
  (cond
    [(hash-ref memo depth #f) (hash-ref memo depth)]
    [(<= (length numbers) 1) 1]
    [else (let* ([slice (slice-next numbers)]
                 [total (for/sum ([n (in-range 1 (add1 (length slice)))])
                          (valid-steps (drop numbers n) (+ depth n) memo))])
            (hash-set! memo depth total)
            total)]))


(printf "part 1: ~a~n" (~> "input" (read-input) (steps) (count-diffs)))
(printf "part 2: ~a~n" (~> "input" (read-input) (cons 0 _) (valid-steps)))
