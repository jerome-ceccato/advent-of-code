#lang racket

(require threading)

(define (tokenize line)
  (regexp-match* #px"\\s*(\\d+|\\+|\\*|\\(|\\))\\s*" line #:match-select cadr))

(define (tokens-to-tree expr [tree (list (list))])
  (if (empty? expr) (reverse (car tree))
      (tokens-to-tree
       (cdr expr)
       (match (car expr)
         ["(" (cons (list) tree)]
         [")" (cons (cons (reverse (car tree)) (cadr tree)) (cddr tree))]
         [(or "+" "*") (cons (cons (car expr) (car tree)) (cdr tree))]
         [n (cons (cons (string->number n) (car tree)) (cdr tree))]))))

(define read-input
  (λ~>>
   (file->lines)
   (map tokenize)
   (map tokens-to-tree)))

(define (eval-op eval-rule lhs op rhs)
  (let ([ln (if (number? lhs) lhs (eval-rule lhs))]
        [rn (if (number? rhs) rhs (eval-rule rhs))])
    (match op
      ["+" (+ ln rn)]
      ["*" (* ln rn)])))

(define (eval-expr expr)
  (if (number? expr) expr
      (for/fold ([acc (car expr)])
                ([i (in-range 1 (length expr) 2)])
        (eval-op eval-expr acc (list-ref expr i) (list-ref expr (add1 i))))))

(define (eval-expr-precedence expr)
  (cond
    [(number? expr) expr]
    [(= 1 (length expr)) (car expr)]
    [else (let* ([opidx (or (index-of expr "+") (index-of expr "*"))]
                 [res (eval-op
                       eval-expr-precedence
                       (list-ref expr (sub1 opidx))
                       (list-ref expr opidx)
                       (list-ref expr (add1 opidx)))])
            (eval-expr-precedence (append
                                   (take expr (sub1 opidx))
                                   (list res)
                                   (drop expr (+ 2 opidx)))))]))

(define (eval-all lst eval-rule)
  (~>>
   lst
   (map eval-rule)
   (foldl + 0)))


(printf "part 1: ~a~n" (~> "input" (read-input) (eval-all eval-expr)))
(printf "part 2: ~a~n" (~> "input" (read-input) (eval-all eval-expr-precedence)))
