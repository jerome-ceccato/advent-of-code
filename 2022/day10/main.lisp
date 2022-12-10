;;; Build registry
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
  (reverse (reduce #'eval-instruction instructions :initial-value (list 0))))


;;; Part 1
(defun take-many (from indices)
    (if (null indices) '() 
      (cons (nth (- (car indices) 1) from) (take-many from (cdr indices)))))

(defun part1 (registry-values)
  (let* ((signal-seq '(20 60 100 140 180 220))
         (target-values (take-many registry-values signal-seq))
         (signal-strengths (mapcar #'* target-values signal-seq)))
    (reduce #'+ signal-strengths)))


;;; Part 2
(defun part2 (registry-values)
  (labels ((itol (n) (loop for i below n collect i))
           (get-pixel (reg idx) (<= (abs (- reg (mod idx 40))) 1))
           (pretty-print (pixel idx)
            (concatenate 'string (if (= 0 (mod idx 40)) "~%" "") (if pixel "#" "."))))
    (let* ((indices (itol (length registry-values)))
           (pixels (mapcar #'get-pixel registry-values indices))
           (pretty-pixels (mapcar #'pretty-print pixels indices))
           (output (reduce (lambda (a b) (concatenate 'string a b)) pretty-pixels)))
      (format nil output))))

;;; Output
(let* ((input (read-input "input"))
       (initial-registry-val 1)
       (cycles (registry-cycles-seq input))
       (registry-values (map 'list (lambda (x) (+ initial-registry-val x)) cycles)))
  (print (part1 registry-values))
  (print (part2 registry-values)))   
