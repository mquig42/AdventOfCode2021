;;;AoC_12.rkt
;;;2021-12-12
;;;Mike Quigley

;;;Today we're pathfinding through a graph. First step is to build some
;;;representation of that graph. Decided on a hash table where the key is a cave
;;;and the value is a list of connected locations.

;;;To enumerate all potential paths, I have a function that takes a
;;;starting point (or current location) and the path taken to reach that point
;;;It recursively calls itself for each potential next cave to visit
;;;and appends the resulting lists together.
;;;There are two different filter functions for determing the next
;;;caves to visit, so the same enumeration function can work for both parts

#lang racket
;Reads each line of the input file
(define (read-lines file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string-trim line) (read-lines file)))))

;Makes a set of all distinct caves, including start and end
(define (get-caves caves input)
  (cond ((null? input) caves)
        (else
         (let ((sp (string-split (car input) "-")))
           (get-caves (set-union caves (set (car sp) (cadr sp)))
                      (cdr input))))))

;Check for a connection between two caves
(define (connection-exists? cave-a cave-b)
  (let ((str-a (string-join (list cave-a cave-b) "-"))
        (str-b (string-join (list cave-b cave-a) "-")))
    (or (member str-a lines) (member str-b lines))))

;Get a list of all caves connected to cave
(define (get-connections cave)
  (filter (λ (x) (connection-exists? cave x)) caves))

;2 functions to check string case
(define (lower? str)
  (string=? (string-downcase str) str))
(define (upper? str)
  (string=? (string-upcase str) str))

;Enumerate all paths to end from given starting point
(define (enumerate-all-paths start path valid?)
  (let ((path (cons start path)))
    (if (string=? start "end") (list path)
        (foldl (λ (x acc)
                 (append acc (enumerate-all-paths x path valid?)))
               null
               (filter (λ (x) (valid? x path)) (hash-ref connections start))))))

;Counts the maximum number of times a distinct lowercase string appears in path
(define (small-duplicates path)
  (argmax identity
          (map (λ (cave)
                 (count (λ (x) (string=? cave x)) path))
               (filter lower? path))))
  
;Is dest a valid cave to visit? By part 1 rules, this is only false for
;small caves that have been visited already
(define (valid-dest-1? dest path)
  (not (and (lower? dest) (member dest path))))

;Is dest a valid cave to visit according to part 2 rules?
;This allows visiting a small cave twice, as long as no small caves
;have already been visited twice
(define (valid-dest-2? dest path)
  (cond ((string=? dest "start") false)
        ((upper? dest) true)
        ((not (member dest path)) true)
        ((< (small-duplicates path) 2) true)
        (else false)))

;Display more detailed output. Instead of just printing the
;number of paths considered, also show the shortest path and its length
(define (display-output paths)
  (let ((shortest (argmin length paths)))
    (display "Number of paths: ")
    (display (length paths))
    (newline)
    (display "  Shortest path: ")
    (display (reverse shortest))
    (newline)
    (display"         Length: ")
    (display (length shortest))
    (newline)))

;;Open file and read input
(define input-file (open-input-file "Input12.txt"))
(define lines (read-lines input-file))
(close-input-port input-file)

;;Make a list of all caves and a hash table of connections between caves
(define caves (set->list (get-caves (set) lines)))
(define connections (make-immutable-hash
                     (map (λ (x) (cons x (get-connections x)))
                          caves)))

;;Print output
(display "            -- Part 1 --")
(newline)
(display-output (enumerate-all-paths "start" null valid-dest-1?))
(newline)
(display "            -- Part 2 --")
(newline)
(display-output (enumerate-all-paths "start" null valid-dest-2?))
