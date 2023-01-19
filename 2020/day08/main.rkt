#lang racket

(require threading)

(define read-input
  (λ~>>
   (file->lines)
   (map
    (λ~>>
     (string-split)
     ((λ (line) (vector (car line) (string->number (cadr line)))))))))

(define (exec-until-loop instructions [store (set)] [current 0] [acc 0])
  (cond
    [(set-member? store current) (list #f acc)]
    [(> current (length instructions)) (list #f 0)]
    [(= current (length instructions)) (list #t acc)]
    [else
     (match-let ([(vector instruction value) (list-ref instructions current)]
                 [next-store (set-add store current)])
       (match instruction
         ["nop" (exec-until-loop instructions next-store (add1 current) acc)]
         ["acc" (exec-until-loop instructions next-store (add1 current) (+ acc value))]
         ["jmp" (exec-until-loop instructions next-store (+ current value) acc)]))]))

(define (list-replacing-elem lst index elem)
  (append (take lst index)
          (list elem)
          (drop lst (add1 index))))

(define (try-swap instructions index)
  (match-let ([(vector instruction value) (list-ref instructions index)])
    (match instruction
      ["acc" (list #f 0)]
      ["nop" (exec-until-loop (list-replacing-elem instructions index (vector "jmp" value)))]
      ["jmp" (exec-until-loop (list-replacing-elem instructions index (vector "nop" value)))])))

(define (fix-program instructions)
  (for/first ([idx (in-range (length instructions))]
              #:when (car (try-swap instructions idx)))
    (cadr (try-swap instructions idx))))


(printf "part 1: ~a~n" (~> "input" (read-input) (exec-until-loop) (cadr)))
(printf "part 2: ~a~n" (~> "input" (read-input) (fix-program)))
