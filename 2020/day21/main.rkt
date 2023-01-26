#lang racket

(require threading)

(define (parse-line line)
  (match-let ([(list ingredients allergens) (string-split line "(contains ")])
    (list
     (string-split ingredients)
     (regexp-match* #px"\\s*(\\w+),?\\)?\\s*" allergens #:match-select cadr))))

(define read-input
  (λ~>>
   (file->lines)
   (map parse-line)))

(define (injest-recipe! mapping recipe)
  (match-let ([(list ingredients allergens) recipe])
    (for ([allergen allergens])
      (if (hash-ref mapping allergen #f)
          (hash-set! mapping allergen (set-intersect (hash-ref mapping allergen) (list->set ingredients)))
          (hash-set! mapping allergen (list->set ingredients))))))

(define (find-mapping input)
  (let ([mapping (make-hash)])
    (for-each (λ~>> (injest-recipe! mapping)) input)
    mapping))

(define (count-regular-ingredients input)
  (let* ([mapping (find-mapping input)]
         [possible-allergens (list->set (flatten (map set->list (hash-values mapping))))])
    (for*/sum ([recipe input]
               [ingredient (car recipe)]
               #:when (not (set-member? possible-allergens ingredient)))
      1)))

(define (remove-known-element mapping elem)
  (for/hash ([key (hash-keys mapping)])
    (values
     key
     (if (> (set-count (hash-ref mapping key)) 1)
         (set-remove (hash-ref mapping key) elem)
         (hash-ref mapping key)))))

(define (reduce-mapping mapping)
  (if (findf (λ (s) (> (set-count s) 1)) (hash-values mapping))
      (reduce-mapping
       (for/fold ([m mapping])
                 ([key (hash-keys mapping)]
                  #:when (= (set-count (hash-ref m key)) 1))
         (remove-known-element m (set-first (hash-ref m key)))))
      mapping))

(define (output-allergens mapping)
  (~>>
   mapping
   (hash-keys)
   (sort _ string<=?)
   (map (λ~>> (hash-ref mapping) (set-first)))
   (string-join _ ",")))

(printf "part 1: ~a~n" (~> "input" (read-input) (count-regular-ingredients)))
(printf "part 2: ~a~n" (~> "input" (read-input) (find-mapping) (reduce-mapping) (output-allergens)))
