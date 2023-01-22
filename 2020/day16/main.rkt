#lang racket

(require threading racket/struct)

(struct data (fields my-ticket nearby-tickets)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (_) 'data)
      (lambda (obj) (list (data-fields obj) (data-my-ticket obj) (data-nearby-tickets obj)))))])

(define (parse-field line)
  (match-let* ([(list name ranges) (string-split line ": ")]
               [(list ar br) (string-split ranges " or ")])
    (list
     name
     (list
      (~> ar (string-split "-") (map string->number _))
      (~> br (string-split "-") (map string->number _))))))

(define (read-input filename)
  (let ([sections (string-split (file->string filename) "\n\n")])
    (data
     (for/hash ([line (string-split (first sections) "\n")]) (apply values (parse-field line)))
     (~> (second sections) (string-split "\n") (second) (string-split ",") (map string->number _))
     (~> (third sections) (string-split "\n") (cdr) (map (λ~> (string-split ",") (map string->number _)) _)))))


(define (valid-for-field n fd)
  (or (<= (first (first fd)) n (second (first fd)))
      (<= (first (second fd)) n (second (second fd)))))

(define (valid-for-any-field n allf)
  (for/or ([fd (hash-values allf)]) (valid-for-field n fd)))

(define (ticket-errors notes)
  (for*/list ([other (data-nearby-tickets notes)]
              [n other]
              #:when (not (valid-for-any-field n (data-fields notes))))
    n))

(define (ticket-error-rate notes)
  (foldl + 0 (ticket-errors notes)))

(define (valid-tickets notes)
  (for/list ([other (data-nearby-tickets notes)]
             #:when (for/and ([n other]) (valid-for-any-field n (data-fields notes))))
    other))

(define (filter-invalid-tickets notes)
  (data (data-fields notes) (data-my-ticket notes) (valid-tickets notes)))

; I originally tried to keep a list of valid rotations for the fields and remove ones that don't work
; for each nearby tickets, only considering valid rotations each time.
; This works fine for the example but the permutations for the real input are 20! (~2e18)

#;(define (ticket-matches notes ticket perm)
    (for/and ([n ticket]
              [fname perm])
      (valid-for-field n (hash-ref (data-fields notes) fname))))

#;(define (valid-fields-permutation notes ticket allperm)
    (display (length allperm))
    (filter (λ~>> (ticket-matches notes ticket)) allperm))

#;(define (find-field-order notes)
    (foldl
     (curry valid-fields-permutation notes)
     (permutations (hash-keys (data-fields notes)))
     (cons (data-my-ticket notes) (data-nearby-tickets notes))))

;;;

(define (valid-fields notes n fields)
  (filter (λ (fd) (valid-for-field n (hash-ref (data-fields notes) fd))) fields))

(define (find-possibilities notes i)
  (let* ([tickets (cons (data-my-ticket notes) (data-nearby-tickets notes))]
         [vals (map (λ~> (list-ref i)) tickets)])
    (foldl (curry valid-fields notes) (hash-keys (data-fields notes)) vals)))

; for each ticket number position, list all fields that could match
(define (get-positional-possibilities notes)
  (for/list ([n (in-range (hash-count (data-fields notes)))])
    (find-possibilities notes n)))

; fold the possibilities by elimating the ones that are already known until all field positions are known
(define (reduce-possibilities possibilities)
  (let* ([locked (~>> possibilities (filter (λ (l) (= 1 (length l)))) (flatten))]
         [pass (map (λ (l)
                      (if (= 1 (length l)) l
                          (filter (λ (fd) (not (member fd locked))) l))) possibilities)])
    (if (= (length locked) (length possibilities))
        locked
        (reduce-possibilities pass))))

(define (find-departure-values notes field-order)
  (for/list ([fd field-order]
             [i (in-naturals)]
             #:when (string-prefix? fd "departure"))
    (list-ref (data-my-ticket notes) i)))

(define (find-ticket-value notes)
  (~>
   notes
   (filter-invalid-tickets)
   (get-positional-possibilities)
   (reduce-possibilities)
   (find-departure-values notes _)
   (foldl * 1 _)))


(printf "part 1: ~a~n" (~> "input" (read-input) (ticket-error-rate)))
(printf "part 2: ~a~n" (~> "input" (read-input) (find-ticket-value)))
