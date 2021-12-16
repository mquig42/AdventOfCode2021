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

;;;Third implementation uses a priority queue for unvisited points
;;;For some reason, this is actually slower than before
;;;It's because heap-remove! takes linear time. Replaced it with heap-remove-eq!
;;;and it's quite a lot faster (230ms for part 1, vs 5s)

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

;Returns the value of the given point in grid. false for invalid coords
(define (get-value grid row column max-row max-col)
  (cond ((and (>= row 0)
              (>= column 0)
              (< row max-row)
              (< column max-col))
         (list-ref (list-ref grid row) column))
        (else false)))

;;Priority queue functions
;Creates a queue element, which is a pair of priority and value
;In this program, value is the ID of a point and priority is distance
(define (q-element priority value)
  (cons priority value))

;Compares two queue elements
(define (q<= x y)
  (<= (car x) (car y)))

;Inits a queue of given size. All elements will have priority of inf
(define (make-q size)
  (let ((q (make-heap q<=)))
    (for-each (λ (x) (heap-add! q (q-element +inf.0 x)))
              (range 1 size))
    q))

;Changes an element's priority
(define (reduce-q-priority queue value from to)
  (heap-remove-eq! queue (q-element from value))
  (heap-add! queue (q-element to value)))

;Returns value of element with lowest priority, and removes it from queue
(define (get-q-lowest queue)
  (let ((lowest (heap-min queue)))
    (heap-remove! queue lowest)
    (cdr lowest)))

;;Finds the total risk of the best path from top left to bottom right of grid
;;Includes Dijkstra's algorithm and some helper functions for getting data
(define (solve input)
  (let* ((max-row (length input))
         (max-col (length (car input)))
         (max-n (* max-row max-col))
         (distances (make-vector max-n +inf.0))
         (unvisited (make-q max-n)))
    
    ;;Getters
    ;It may be helpful to have a unique ID for each point in the grid
    ;as a single value, not a coordinate pair. Use an integer, like this
    ;'(0 1 2)
    ;'(3 4 5)
    ;'(6 7 8)
    (define (get-value-n n)
      (get-value input
                 (floor (/ n max-col))
                 (modulo n max-col)
                 max-row max-col))

    ;Calculate ID number for row and column
    (define (get-n row column)
      (+ (* row max-col) column))

    ;Get a list of neighbouring id numbers
    (define (get-neighbours n)
      (let ((row (floor (/ n max-col)))
            (col (modulo n max-col)))
        (map (λ (x) (get-n (car x) (cdr x)))
             (filter (λ (y) (get-value input (car y) (cdr y) max-row max-col))
                     (list (cons (- row 1) col)
                           (cons (+ row 1) col)
                           (cons row (- col 1))
                           (cons row (+ col 1)))))))

    ;Dijkstra's Algorithm for shortest path
    (define (dijkstra current dest)
      (for-each (λ (x)
                  (let ((from (vector-ref distances x))
                        (to (+ (vector-ref distances current)
                               (get-value-n x))))
                    (cond ((> from to)
                           (vector-set! distances x to)
                           (reduce-q-priority unvisited x from to)))))
                (get-neighbours current))
      (if (= current dest) (vector-ref distances current)
            (dijkstra (get-q-lowest unvisited) dest)))

    (vector-set! distances 0 0)
    (dijkstra 0 (- max-n 1))))

;;Generate the larger grid for part 2
(define (extend grid)
  (let ((max-row (length grid))
        (max-col (length (car grid))))
    (define (wrap n)
      (+ 1 (modulo (- n 1) 9)))
    (define (risk-modifier row col)
      (+ (floor (/ col max-col)) (floor (/ row max-row))))
    (define (extend-row grid row)
      (map (λ (x) (wrap (+ (risk-modifier row x)
                           (get-value grid (modulo row max-row)
                                      (modulo x max-col)
                                      max-row
                                      max-col))))
           (range (* max-col 5))))
    (map (λ (y) (extend-row grid y)) (range (* max-row 5)))))

;;Read input
(define input-file (open-input-file "Input15.txt"))
(define input (read-lines input-file))
(close-input-port input-file)

;;Display output
(display "Part 1: ")
(solve input)
(display "Part 2: ")
(solve (extend input))
