#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->string)
   (string-split _ "\n\n")))

(define count-unique
  (λ~>>
   (map
    (λ~>
     (string-replace "\n" "")
     (string->list)
     (list->set)))
   (map set-count)
   (foldl + 0)))

(define count-common
  (λ~>>
   (map
    (λ~>>
     (string-split)
     (map
      (λ~>
       (string->list)
       (list->set)))
     ((λ (x) (foldl set-intersect (car x) (cdr x))))))
   (map set-count)
   (foldl + 0)))

(printf "part 1: ~a~n" (~> "input" (read-input) (count-unique)))
(printf "part 2: ~a~n" (~> "input" (read-input) (count-common)))
