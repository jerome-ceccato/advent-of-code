#lang racket

(require threading)

(define (parse-rule line)
  (match-let ([(list name rule) (string-split line ":")])
    (list name (map string-split (string-split rule "|")))))

(define (read-input filename)
  (let ([sections (string-split (file->string filename) "\n\n")])
    (list
     (for/hash ([line (string-split (first sections) "\n")]) (apply values (parse-rule line)))
     (~> (second sections) (string-split "\n")))))


(define (filter-acceptable candidates c)
  (~>>
   candidates
   (filter (λ~> (string-prefix? c)))
   (map (λ~> (substring 1)))))

(define (match-path rules line path)
  (for/fold ([candidates (list line)])
            ([item path] #:when (not (empty? candidates)))
    (match item
      ["\"a\"" (filter-acceptable candidates "a")]
      ["\"b\"" (filter-acceptable candidates "b")]
      [rule-id (flatten (map (λ (c) (try-match rules c rule-id)) candidates))])))

; returns a list of all possible suffixes of line after matching the current-rule
(define (try-match rules line current-rule)
  (flatten (for/list ([path (hash-ref rules current-rule)])
             (match-path rules line path))))

(define (try-all data)
  (~>>
   (second data)
   (map (λ~> (try-match (first data) _ "0")))
   ; only empty strings count as a full match
   (count (λ~>> (member "")))))

(define (fix-data data)
  (list
   (~>
    (first data)
    (hash-set "8" (list (list "42") (list "42" "8")))
    (hash-set "11" (list (list "42" "31") (list "42" "11" "31"))))
   (second data)))


(printf "part 1: ~a~n" (~> "input" (read-input) (try-all)))
(printf "part 2: ~a~n" (~> "input" (read-input) (fix-data) (try-all)))
