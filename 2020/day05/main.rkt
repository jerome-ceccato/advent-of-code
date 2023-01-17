#lang racket

(require threading)

(define (parse-binary str false true)
  (~>
   str
   (string-replace false "0")
   (string-replace true "1")
   (string-append "#b" _)
   (call-with-input-string read)))

(define (parse-line line)
  (+
   (~> line (substring 0 7) (parse-binary "F" "B") (* 8))
   (~> line (substring 7) (parse-binary "L" "R"))))

(define read-input
  (λ~>>
   (file->lines)
   (map parse-line)))

(define (find-seat seats)
  (let ([sorted (sort seats <)])
    (for/first ([i sorted]
                [j (cdr sorted)]
                #:when (not (equal? (add1 i) j)))
      (add1 i))))

(printf "part 1: ~a~n" (~> "input" (read-input) (apply max _)))
(printf "part 2: ~a~n" (~> "input" (read-input) (find-seat)))
