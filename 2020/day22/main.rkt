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

(define (play-rec-normal-round c1 c2 p1 p2)
  (if (> c1 c2)
      (list (append p1 (list c1 c2)) p2)
      (list p1 (append p2 (list c2 c1)))))

(define (play-rec-recursive-round c1 c2 p1 p2)
  (let* ([p1-copy (take p1 c1)]
         [p2-copy (take p2 c2)]
         [rec-game-result (play-rec (list p1-copy p2-copy))])
    (if (eqv? (car rec-game-result) 'p1)
        (list (append p1 (list c1 c2)) p2)
        (list p1 (append p2 (list c2 c1))))))

(define (play-rec decks [game-cache (set)])
  (match-let ([(list p1 p2) decks])
    (cond
      [(empty? p1) (list 'p2 p1 p2)]
      [(empty? p2) (list 'p1 p1 p2)]
      [(set-member? game-cache decks) (list 'p1 p1 p2)]
      [else
       (let ([c1 (car p1)] [c2 (car p2)])
         (cond
           [(or (> c1 (length (cdr p1))) (> c2 (length (cdr p2))))
            (play-rec (play-rec-normal-round c1 c2 (cdr p1) (cdr p2)) (set-add game-cache decks))]
           [else
            (play-rec (play-rec-recursive-round c1 c2 (cdr p1) (cdr p2)) (set-add game-cache decks))]))])))


(printf "part 1: ~a~n" (~> "input" (read-input) (play) (score)))
(printf "part 2: ~a~n" (~> "input" (read-input) (play-rec) (cdr) (score)))
