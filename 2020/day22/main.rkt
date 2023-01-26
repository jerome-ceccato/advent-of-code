#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->string)
   (string-split _ "\n\n")
   (map
    (λ~>>
     (string-split _ "\n")
     (cdr)
     (map string->number)))))

(define (play-round p1 p2)
  (if (> (car p1) (car p2))
      (list
       (append (cdr p1) (list (car p1) (car p2)))
       (cdr p2))
      (list
       (cdr p1)
       (append (cdr p2) (list (car p2) (car p1))))))

(define (play decks)
  (match-let ([(list p1 p2) decks])
    (if (or (empty? p1) (empty? p2))
        decks
        (play (play-round p1 p2)))))

(define (score decks)
  (let ([winner (findf (negate empty?) decks)])
    (for/sum ([n (reverse winner)]
              [i (in-naturals 1)])
      (* n i))))

(printf "part 1: ~a~n" (~> "input" (read-input) (play) (score)))
;(printf "part 2: ~a~n" (~> "input" (read-input)))
