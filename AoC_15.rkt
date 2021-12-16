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

;;;Second implementation, using a vector to store distances, takes 5361ms
;;;to solve part 1. That's a significant improvement, but it could be better.
;;;Still not confident about part 2.
;;;Let's see if I can use a priority queue to speed up finding the next node
;;;to visit.

#lang racket
(require data/heap)

;;Generate starting grid by reading file
(define (read-lines file)
  (define (string->digits str)
    (map (λ (x) (- (char->integer x) (char->integer #\0)))
         (string->list str)))
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string->digits (string-trim line))
              (read-lines file)))))

;;Finds the total risk of the best path from top left to bottom right of grid
;;Includes Dijkstra's algorithm and some helper functions for getting data
(define (solve input)
  (let ((max-row (length input))
        (max-col (length (car input)))
        (max-n (* (length input) (length (car input))))
        (distances (make-vector
                    (* (length input) (length (car input))) +inf.0)))
    
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
    (define (dijkstra current dest unvisited)
      (for-each (λ (x) (vector-set! distances x
                                    (min (vector-ref distances x)
                                         (+ (vector-ref distances current)
                                            (get-value-n x)))))
                (get-neighbours current))
      (if (= current dest) (vector-ref distances current)
          (let ((unvisited-c (set-remove unvisited current)))
            (dijkstra (argmin (λ (x) (vector-ref distances x))
                              (set->list unvisited-c))
                      dest
                      unvisited-c))))

    (vector-set! distances 0 0)
    (dijkstra 0 (- max-n 1) (list->set (range max-n)))))

;;Read input
(define input-file (open-input-file "Input15.txt"))
(define input (read-lines input-file))
(close-input-port input-file)

;;Display output
(display "Part 1: ")
(solve input)
