(import (rnrs))

;;; General purpose helpers

; https://cookbook.scheme.org/split-string/
(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))

; check if a string starts with a prefix
(define (string-prefix? prefix str)
  (and 
    (>= (string-length str) (string-length prefix))
    (string=? prefix (substring str 0 (string-length prefix)))))

; get a list of string for each line in a file
(define (read-input filename)
  (call-with-input-file filename
    (lambda (port)
      (letrec ([read-lines (lambda ()
        (let ([line (get-line port)])
          (if (port-eof? port)
            (list line)
            (cons line (read-lines)))))])
        (read-lines)))))

; calls func n times
(define (times n func)
  (if (> n 0)
    (begin (func) (times (- n 1) func))
    0))

; checks if an element already exists in a list
(define (list-contains list predicate)
  (cond 
    ((null? list) #f)
    ((predicate (car list)) #t)
    (else (list-contains (cdr list) predicate))))


;;; Records

; filesystem record
(define-record-type (fs make-fs fs?)
  (fields (immutable pwd fs-pwd) ; reverse order path (head is last path element)
          (immutable dirs fs-dirs))) ; list of dir

; directory record
(define-record-type (dir make-dir dir?)
  (fields (immutable name dir-path) ; needs to be the full path to be unique
          (immutable subdirs dir-subdirs) ; list of dir names (string)
          (immutable files dir-files))) ; list of file

; file record
(define-record-type (file make-file file?)
  (fields (immutable name file-name)
          (immutable size file-size)))


; Filesystem functions

; remove the last path element if possible (head of path list)
(define (cut-path pwd)
  (if (null? pwd)
    pwd
    (cdr pwd)))

; checks equality in dir path
(define (dir-path=? a b)
  (cond 
    ((null? a) (null? b))
    ((null? b) #f)
    ((string=? (car a) (car b)) (dir-path=? (cdr a) (cdr b)))
    (else #f)))

; rebuilds a list of dir by transforming one element
; adds a new dir if needed
(define (transform-dir dirs pwd transform)
    (cond 
      ((null? dirs) 
        (list (transform (make-dir pwd '() '())))) ; not found, add a new one
      ((dir-path=? pwd (dir-path (car dirs)))
        (cons (transform (car dirs)) (cdr dirs))) ; found, transform and append the rest of the list
      (else (cons (car dirs) (transform-dir (cdr dirs) pwd transform))))) ; build list rec

; transforms the current pwd, rebuilding a new fs
(define (transform-current-dir fs transform)
  (make-fs (fs-pwd fs) 
           (transform-dir (fs-dirs fs) (fs-pwd fs) transform)))


; add a dir reference to the current fs
(define (add-dir fs dir)
  (transform-current-dir fs (lambda (old)
    (if (list-contains (dir-subdirs old) (lambda (x) (string=? x dir)))
      old
      (make-dir (dir-path old)
              (cons dir (dir-subdirs old))
              (dir-files old))))))

; add a file to the current fs
(define (add-file fs file)
  (transform-current-dir fs (lambda (old)
    (if (list-contains (dir-files old) (lambda (x) (string=? (file-name x) (file-name file))))
      old
      (make-dir (dir-path old)
                (dir-subdirs old)
                (cons file (dir-files old)))))))

; add an ls entry in the filesystem record
(define (apply-ls fs line)
  (let ([split (string-split char-whitespace? line)])
    (if (string=? (car split) "dir")
      (add-dir fs (car (cdr split)))
      (let ([file (make-file (car (cdr split)) (string->number (car split)))])
        (add-file fs file)))))

; apply an input command
(define (apply-command fs line)
  (cond
    ((string=? line "$ cd /") 
      (make-fs '("/") (fs-dirs fs))) ; reset pwd
    ((string=? line "$ cd ..") 
      (make-fs (cut-path (fs-pwd fs)) (fs-dirs fs))) ; remove last element from pwd
    ((string-prefix? "$ cd " line) 
      (make-fs (cons (substring line 5 (string-length line)) (fs-pwd fs)) (fs-dirs fs))) ; adds argument to pwd
    ((string=? line "$ ls") fs) ; do nothing, lines not recognized are assumed to be ls output
    (else (apply-ls fs line))))

; get dir record from fs
(define (get-dir fs path)
  (letrec ([find-dir (lambda (dirs path) 
    (cond
      ((null? dirs) '()) ; should never happen, unknown dir
      ((dir-path=? path (dir-path (car dirs))) (car dirs))
      (else (find-dir (cdr dirs) path))))])
    (find-dir (fs-dirs fs) path)))

; pretty-print the fs record
(define (display-fs fs)
  (letrec ([show-dir (lambda (fs dirpath depth)
    (let ([dir (get-dir fs dirpath)])
      (times (* 2 depth) (lambda () (display " ")))
      (display "- ")
      (display (car (dir-path dir)))
      (display " (dir)")
      (newline)
      (for-each (lambda (d) (show-dir fs (cons d dirpath) (+ 1 depth))) (dir-subdirs dir))
      (for-each (lambda (file)
        (times (* 2 (+ 1 depth)) (lambda () (display " ")))
        (display "- ")
        (display (file-name file))
        (display " (file, size=")
        (display (file-size file))
        (display ")")
        (newline))
        (dir-files dir))
      ))])
    (show-dir fs '("/") 0)))

; build a filesystem record from command lines
(define (build-fs lines)
  (fold-left apply-command (make-fs '() '()) lines))


;;; Solution

; total size of a directory, including all its subdirectories and files
(define (dir-total-size fs dir)
  (+ (fold-left (lambda (acc file) (+ acc (file-size file))) 0 (dir-files dir))
     (fold-left (lambda (acc dirname) 
      (+ acc (dir-total-size fs (get-dir fs (cons dirname (dir-path dir))))))
     0 (dir-subdirs dir))))

; find and add the size of all directories under the limit
(define (find-small-directories fs limit)
  (let ([dir-size-limited (lambda (dir) 
    (let ([total (dir-total-size fs dir)])
      (if (< total limit) total 0)))])
    (fold-left (lambda (acc dir) (+ acc (dir-size-limited dir))) 0 (fs-dirs fs))))

; return the total size of all distinct directories in the fs
(define (all-dir-sizes fs)
  (map (lambda (dir) (dir-total-size fs dir)) (fs-dirs fs)))

; finds the minimum sized directory to delete to meet target size
(define (free-space sizes root-size target)
  (letrec ([search (lambda (sizes res) 
    (cond
      ((null? sizes) res)
      (else (search (cdr sizes) 
        (if (>= (car sizes) target)
          (min res (car sizes))
          res)))))])
    (search sizes root-size)))


(let ([fs (build-fs (read-input "input"))])
  (display-fs fs)
  (newline)
  ; part 1
  (display (find-small-directories fs 100000))
  (newline)

  ; part 2
  (let ([sizes (all-dir-sizes fs)]
        [root-size (dir-total-size fs (get-dir fs '("/")))])
    (let ([target (- 30000000 (- 70000000 root-size))])
      (display (free-space sizes root-size target))
      (newline))))
