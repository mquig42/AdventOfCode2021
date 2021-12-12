;;;AoC_12.rkt
;;;2021-12-12
;;;Mike Quigley

;;;Today we're pathfinding through a graph. First step is to build some
;;;representation of that graph. Maybe a hash table where the key is a cave
;;;and the value is a list of connected locations.

;;;Paths can't pas through a lowercase-named cave more than once, and
;;;I will assume that rule is there to prevent cycles

;;;There's probably a way to use recursive mapping and filtering to
;;;enumerate all paths. Would require some thought.

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

;Checks if str is lowercase
(define (lower? str)
  (string=? (string-downcase str) str))

;Enumerate all paths to end from given starting point
(define (enumerate-all-paths start path valid?)
  (if (string=? start "end") (list (cons start path))
      (foldl (λ (x acc)
               (append acc (enumerate-all-paths x (cons start path) valid?)))
             null
             (filter (λ (x) (valid? x path)) (hash-ref connections start)))))

(define (valid-dest-1? dest path)
  (not (and (lower? dest) (member dest path))))

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
(display "Part 1: ")
(length (enumerate-all-paths "start" null valid-dest-1?))