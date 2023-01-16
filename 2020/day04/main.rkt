#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->string)
   (string-split _ "\n\n")
   (map
    (λ~>>
     (string-split _ #px"\\s")
     (map (λ~> (string-split ":")))))))

(define (count-valid input filtering)
  (for/sum ([item input])
    (~>>
     item
     (filter filtering)
     (length)
     (= 7)
     (if _ 1 0))))

(define (filter-cid pair)
  (not (equal? "cid" (car pair))))

(define (filter-all pair)
  (match pair
    [(list "byr" x) (<= 1920 (string->number x) 2002)]
    [(list "iyr" x) (<= 2010 (string->number x) 2020)]
    [(list "eyr" x) (<= 2020 (string->number x) 2030)]
    [(list "hgt" cm)
     #:when (string-suffix? cm "cm")
     (<= 150 (string->number (substring cm 0 (- (string-length cm) 2))) 193)]
    [(list "hgt" in)
     #:when (string-suffix? in "in")
     (<= 59 (string->number (substring in 0 (- (string-length in) 2))) 76)]
    [(list "hcl" x) (regexp-match-exact? #px"#[0-9a-fA-F]{6}" x)]
    [(list "ecl" x) (if (member x (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth")) #t #f)]
    [(list "pid" x) (regexp-match-exact? #px"\\d{9}" x)]
    [_ #f]))

(printf "part 1: ~a~n" (~> "input" (read-input) (count-valid filter-cid)))
(printf "part 2: ~a~n" (~> "input" (read-input) (count-valid filter-all)))
