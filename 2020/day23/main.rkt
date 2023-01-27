#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->string)
   (string-trim)
   (string->list)
   (map string)
   (map string->number)))

(define (next-dest-idx lst current)
  (let* ([prev (if (= current 1) (+ 3 (length lst)) (sub1 current))]
         [idx (index-of lst prev)])
    (if (false? idx) (next-dest-idx lst prev) idx)))

; current cup should always be the first
(define (single-round lst)
  (let* ([current (car lst)]
         [pick-up (take (cdr lst) 3)]
         [lst-after-pickup (append (list current) (drop lst 4))]
         [dest-idx (next-dest-idx lst-after-pickup current)]
         [next-lst (append (take lst-after-pickup (add1 dest-idx)) pick-up (drop lst-after-pickup (add1 dest-idx)))]
         [final-lst (append (cdr next-lst) (list (car next-lst)))])
    final-lst))

(define (play lst rounds)
  (for/fold ([l lst]) ([_ (in-range rounds)]) (single-round l)))

(define (rotate-to-one lst)
  (let ([one (index-of lst 1)])
    (append (drop lst (add1 one)) (take lst one))))

(define game->string
  (λ~>> (rotate-to-one) (map number->string) (string-join _ "")))

(define (pad-list lst total)
  (for/list ([i (in-range total)])
    (if (< i (length lst)) (list-ref lst i) (add1 i))))

; My naive solution was way too slow, this uses a dictionary as a linked list
; (each element of the list is a key in the hash and its value is the next element)
; It allows very little shuffling per round, only 3 set operations (+ 1 to keep track of the head)

(define (to-llist lst)
  (let ([v (list->vector lst)])
    (hash-set
     (for/hash ([i (in-range (vector-length v))])
       (values (vector-ref v i) (vector-ref v (modulo (add1 i) (vector-length v)))))
     'head (car lst))))

(define (dest-number ll current a b c)
  (let ([prev (if (= current 1) (sub1 (hash-count ll)) (sub1 current))])
    (if (or (= prev a) (= prev b) (= prev c)) (dest-number ll prev a b c) prev)))

(define (play-round! ll)
  (let* ([current (hash-ref ll 'head)]
         [next1 (hash-ref ll current)]
         [next2 (hash-ref ll next1)]
         [next3 (hash-ref ll next2)]
         [destn (dest-number ll current next1 next2 next3)])
    ; current point to current +4 (skip pick up)
    (hash-set! ll current (hash-ref ll next3))
    ; point last picked up to dest +1
    (hash-set! ll next3 (hash-ref ll destn))
    ; point dest to first picked up
    (hash-set! ll destn next1)
    ; set head to head +1
    (hash-set! ll 'head (hash-ref ll current))
    ll))

(define (play-ll ll rounds)
  (for/fold ([mll (hash-copy ll)])
            ([_ (in-range rounds)])
    (play-round! mll)))

(define (find-stars ll)
  (* (hash-ref ll 1) (hash-ref ll (hash-ref ll 1))))

(printf "part 1: ~a~n" (~> "input" (read-input) (play 100) (game->string)))
(printf "part 2: ~a~n" (~> "input" (read-input) (pad-list 1000000) (to-llist) (play-ll 10000000) (find-stars)))
