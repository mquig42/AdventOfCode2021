;;;AoC_15.rkt
;;;2021-12-15
;;;Mike Quigley

;;;We're trying to avoid molluscs while navigating a small cave (though the
;;;input is 100x100, so it's not that small). Each number in the grid represents
;;;a risk level. Find the path from the top left to bottom right with the
;;;lowest total risk. Can move up, down, left, right, but not diagonally.

;;;First implementation used Dijkstra's algorithm with distances as a list
;;;It ran extremely slowly, taking over 10 minutes to solve part 1.
;;;Needs some optimization, and possibly mutable state (vectors are so much
;;;faster than lists)

#lang racket
;Read input file into a list
(define (read-lines file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string->digits (string-trim line))
              (read-lines file)))))

;Convert a digit string to a list of numbers
(define (string->digits str)
  (map (λ (x) (- (char->integer x) (char->integer #\0)))
       (string->list str)))

;;Getters
;Returns the value of the given point in grid. false for invalid coords
(define (get-value row column)
  (cond ((and (>= row 0)
              (>= column 0)
              (< row max-row)
              (< column max-col))
         (list-ref (list-ref input row) column))
        (else false)))

;It may be helpful to have a unique ID for each point in the grid
;as a single value, not a coordinate pair. Use an integer, like this
;'(0 1 2)
;'(3 4 5)
;'(6 7 8)
(define (get-value-n n)
  (get-value (floor (/ n max-col)) (modulo n max-col)))

;Calculate ID number for row and column
(define (get-n row column)
  (+ (* row max-col) column))

;Get a list of neighbouring id numbers
(define (get-neighbours n)
  (let ((row (floor (/ n max-col)))
        (col (modulo n max-col)))
    (map (λ (x) (get-n (car x) (cdr x)))
         (filter (λ (y) (get-value (car y) (cdr y)))
                 (list (cons (- row 1) col)
                       (cons (+ row 1) col)
                       (cons row (- col 1))
                       (cons row (+ col 1)))))))

;Dijkstra's Algorithm for shortest path
(define (dijkstra current dest distances unvisited)
  (display (set-count unvisited))
  (newline)
  (define (update-distances)
    (let ((neighbours (get-neighbours current)))
      (map (λ (dist id) (if (not (member id neighbours)) dist
                            (min dist (+ (list-ref distances current)
                                         (get-value-n id)))))
           distances (range (length distances)))))
  (if (= current dest) (list-ref distances current)
      (dijkstra (argmin (λ (x) (list-ref distances x)) (set->list unvisited))
                dest
                (update-distances)
                (set-remove unvisited current))))
       

;;Read input
(define input-file (open-input-file "Input15.txt"))
(define input (read-lines input-file))
(close-input-port input-file)
(define max-row (length input))
(define max-col (length (car input)))
(define max-n (* max-row max-col))

(define distances (map (λ (x) (if (= x 0) 0 +inf.0)) (range max-n)))

(dijkstra 0 (- max-n 1) distances (list->set (range max-n)))
