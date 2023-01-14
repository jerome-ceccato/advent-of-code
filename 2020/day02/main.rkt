#lang racket

(require threading)

(struct rule (lower upper c password))

(define read-input
  (λ~>>
   (file->lines)
   (map
    (λ~>>
     (regexp-match #px"(\\d+)-(\\d+) (\\w): (\\w+)")
     (cdr)
     ((λ (x)
        (list
         (string->number (first x))
         (string->number (second x))
         (string-ref (third x) 0)
         (fourth x))))
     (apply rule)))))

(define input (read-input "input"))

(define part1
  (λ~>>
   (count
    (λ (r)
      (~>>
       (string->list (rule-password r))
       (filter (λ (c) (eqv? c (rule-c r))))
       (length)
       ((λ (x) (<= (rule-lower r) x (rule-upper r)))))))))

(define part2
  (λ~>>
   (count
    (λ (r)
      (xor
       (eqv? (rule-c r) (string-ref (rule-password r) (- (rule-lower r) 1)))
       (eqv? (rule-c r) (string-ref (rule-password r) (- (rule-upper r) 1))))))))


(printf "part 1: ~a~n" (part1 input))
(printf "part 2: ~a~n" (part2 input))
