#lang racket

(require threading)

(define (bags-list str)
  (if
   (equal? str "no other bags")
   (list)
   (~>>
    str
    (regexp-match* #px"(\\d+) (\\w+ \\w+) bag" #:match-select cdr)
    (map (λ (x) (vector (string->number (car x)) (cadr x)))))))

(define (read-input filename)
  (for/hash ([line (file->lines filename)])
    (~>>
     line
     (regexp-match #px"(\\w+ \\w+) bags contain ([^.]+|no other bags).")
     (cdr)
     ((λ (x) (values (car x) (bags-list (cadr x))))))))

(define (bag-contains? bags bag target)
  (for/first ([content (hash-ref bags bag)]
              #:when (or
                      (equal? (vector-ref content 1) target)
                      (bag-contains? bags (vector-ref content 1) target)))
    #t))

(define (count-holders bags target)
  (~>
   bags
   (hash-map (λ (key _) (bag-contains? bags key target)))
   (count identity _)))

(define (false-as-0 n)
  (if (eq? n #f) 0 n))

(define (count-contained bags target)
  (false-as-0
   (for/sum ([content (hash-ref bags target)])
     (* (vector-ref content 0) (add1 (count-contained bags (vector-ref content 1)))))))


(define target-bag "shiny gold")
(printf "part 1: ~a~n" (~> "input" (read-input) (count-holders target-bag)))
(printf "part 2: ~a~n" (~> "input" (read-input) (count-contained target-bag)))
