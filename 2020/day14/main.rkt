#lang racket

(require threading)

(define (parse-as-bin mask x)
  (~>
   mask
   (string-replace "X" x)
   (string->number 2)))

; split the mask into a mask for 1s (or) and a mask for 0s (and)
(define (parse-mask mask)
  (list (parse-as-bin mask "0") (parse-as-bin mask "1")))

(define (parse-line line)
  (if
   (string-prefix? line "mask = ")
   (list 'mask
         (~>>
          (string-length "mask = ")
          (substring line)))
   (list 'mem
         (~>>
          line
          (regexp-match* #px"mem\\[(\\d+)\\] = (\\d+)" #:match-select cdr)
          (flatten)
          (map string->number)))))

(define read-input
  (λ~>>
   (file->lines)
   (map parse-line)))


(define (apply-mask mask val)
  (~> val (bitwise-ior (first mask)) (bitwise-and (second mask))))

(define (update-mem! memory mask idx val)
  (hash-set! memory idx (apply-mask mask val))
  memory)

(define (count-mem memory)
  (foldl + 0 (hash-values memory)))

(define (run-v1 instructions [mask '(0 0)] [memory (make-hash)])
  (if (empty? instructions) memory
      (match (car instructions)
        [(list 'mask next-mask) (run-v1 (cdr instructions) (parse-mask next-mask) memory)]
        [(list 'mem (list idx val)) (run-v1 (cdr instructions) mask (update-mem! memory mask idx val))])))

(define (addr-combinations addr variables)
  (if (empty? variables) (list addr)
      (append
       (addr-combinations addr (cdr variables))
       (addr-combinations (bitwise-ior addr (car variables)) (cdr variables)))))

(define (apply-address-mask mask addr)
  (let* ([ones-mask (~> mask (string-replace "X" "0") (string->number 2))]
         [variable-mask (~> mask (string-replace "0" "1") (string-replace "X" "0") (string->number 2))]
         ; set the 1s from the mask and set all X bits to 0
         [masked-addr (~> addr (bitwise-ior ones-mask) (bitwise-and variable-mask))]
         ; get a list of all individual variable bits
         [mask-variable-bits (for/list ([bit (string->list mask)]
                                        [x (in-naturals)] #:when (equal? bit #\X))
                               (arithmetic-shift 1 (- (string-length mask) x 1)))])
    (addr-combinations masked-addr mask-variable-bits)))

(define (update-mem-v2! memory mask addr val)
  (let ([all-mem-addr (apply-address-mask mask addr)])
    (for-each (λ~> (hash-set! memory _ val)) all-mem-addr)
    memory))

(define (run-v2 instructions [mask "0"] [memory (make-hash)])
  (if (empty? instructions) memory
      (match (car instructions)
        [(list 'mask next-mask) (run-v2 (cdr instructions) next-mask memory)]
        [(list 'mem (list idx val)) (run-v2 (cdr instructions) mask (update-mem-v2! memory mask idx val))])))


(printf "part 1: ~a~n" (~> "input" (read-input) (run-v1) (count-mem)))
(printf "part 2: ~a~n" (~> "input" (read-input) (run-v2) (count-mem)))
