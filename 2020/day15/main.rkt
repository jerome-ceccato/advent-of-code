#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->string)
   (string-split _ ",")
   (map string->number)))

(define (play-until game tick target)
  (if (>= tick target) game
      (match-let ([(list last-played positions) game])
        (let ([next (hash-ref positions last-played tick)])
          (play-until (list (- tick next) (hash-set positions last-played tick)) (add1 tick) target)))))

(define (play queue until)
  (play-until
   (list
    (last queue)
    (for/hash ([n (drop-right queue 1)]
               [i (in-naturals 1)])
      (values n i)))
   (length queue)
   until))


(printf "part 1: ~a~n" (~> "input" (read-input) (play 2020) (first)))
(printf "part 2: ~a~n" (~> "input" (read-input) (play 30000000) (first)))
