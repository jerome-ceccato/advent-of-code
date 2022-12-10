(defun read-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun eval-instruction (cycles instruction)
  (cond ((equal "noop" instruction) (cons (car cycles) cycles))
        (t (let ((value (parse-integer (subseq instruction 5)))
                (registry (car cycles)))
            (cons (+ value registry) (cons registry cycles))))))

(defun registry-cycles-seq (instructions)
  (cdr (reverse (reduce #'eval-instruction instructions :initial-value (list 0)))))

(defun take-many (from indices)
    (if (null indices) '() 
      (cons (nth (- (car indices) 2) from) (take-many from (cdr indices)))))

(let* ((input (read-input "input"))
       (signal-seq '(20 60 100 140 180 220))
       (initial-registry-val 1)
       (cycles (registry-cycles-seq input))
       (registry-values (map 'list (lambda (x) (+ initial-registry-val x)) (take-many cycles signal-seq)))
       (signal-strengths (mapcar #'* registry-values signal-seq))
       (total (reduce #'+ signal-strengths)))   
  (print total))
