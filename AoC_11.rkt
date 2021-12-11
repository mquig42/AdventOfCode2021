;;;AoC_11.rkt
;;;2021-12-11
;;;Mike Quigley

;;;Today's input is the energy states of a grid of bioluminescent octopodes
;;;Each step, the level increases by 1. When it reaches 10, the octopus flashes,
;;;reducing its own energy state to 0 and incrementing all its neighbours by 1.
;;;Flashes can cascade, but each octopus remains at 0 after it flashes,
;;;no matter how many neighbours also flashed.
;;;They resume increasing in the next step.

;;;For part 1, count the total number of flashes after 100 steps.

;;;The cascading could be tricky. Try using two grids.
;;;Call the input to the function gridA
;;;Generate a gridB using the following rules:
;;; * Any cell with a value of 0 in gridA is also 0 in gridB
;;; * Other cells are (the value from gridA) + (the number of adjacent gridA cells greater than 9)
;;;
;;;Then use gridA as a mask on gridB to generate gridC
;;; * Any cell that is >9 in gridA is 0 in gridC
;;; * Other cells take their value from gridB
;;;gridC is the input to the next round of tail-recursion.
;;;If you get a gridA that doesn't have any cells >9, return it

;;;For part 2, simply run the simulation until all octopodes are at 0
;;;This is one of those days where part 2 is much easier than part 1

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
(define (get-value grid row column)
  (cond ((and (>= row 0)
              (>= column 0)
              (< row max-row)
              (< column max-col))
         (list-ref (list-ref grid row) column))
        (else false)))

;Returns the values of all cells in grid adjacent to the given coords
(define (get-adjacent-values grid row column)
  (filter identity
          (list (get-value grid (- row 1) (- column 1))
                (get-value grid (- row 1) column)
                (get-value grid (- row 1) (+ column 1))
                (get-value grid row (- column 1))
                (get-value grid row (+ column 1))
                (get-value grid (+ row 1) (- column 1))
                (get-value grid (+ row 1) column)
                (get-value grid (+ row 1) (+ column 1)))))

;Returns the number of adjacent cells that are currently flashing
(define (get-adjacent-flash grid row column)
  (count (λ (x) (> x 9)) (get-adjacent-values grid row column)))

;Returns the number of cells in grid where proc is true
(define (count-grid proc grid)
  (foldl + 0 (map (λ (x) (count proc x)) grid)))

;Processes a flash cascade
(define (flash grid)
  (define (make-grid-b grid-a)
    (define (make-row-b row)
      (define (make-cell-b row column)
        (if (= 0 (get-value grid-a row column)) 0
            (+ (get-value grid-a row column)
               (get-adjacent-flash grid-a row column))))
      (map (λ (x) (make-cell-b row x)) (range max-col)))
    (map (λ (x) (make-row-b x)) (range max-row)))
  
  (define (make-grid-c grid-a grid-b)
    (define (make-row-c row)
      (define (make-cell-c row column)
        (if (> (get-value grid-a row column) 9) 0
            (get-value grid-b row column)))
      (map (λ (x) (make-cell-c row x)) (range max-col)))
    (map (λ (x) (make-row-c x)) (range max-row)))
  
  (if (= 0 (count-grid (λ (x) (> x 9)) grid)) grid
      (flash (make-grid-c grid (make-grid-b grid)))))

;;Simulation functions
;Adds 1 to every number in a 2D list
(define (increment grid)
  (define (inc-line line)
    (map (λ (x) (+ x 1)) line))
  (map (λ (x) (inc-line x)) grid))

;Runs simulation for n steps and returns resulting grid
(define (run-n-steps grid n)
  (if (= 0 n) grid
      (run-n-steps (flash (increment grid)) (- n 1))))

;Runs simulation for n steps and returns total number of flashes (solves part 1)
(define (count-all-flashes grid acc n)
  (if (= 0 n) acc
      (let ((new-grid (flash (increment grid))))
        (count-all-flashes
         new-grid (+ acc (count-grid zero? new-grid)) (- n 1)))))

;Runs simulation until all octopodes flash at the same time.
;Returns number of time steps it took for that to happen (solves part 2)
(define (time-to-sync grid t)
  (if (= (count-grid zero? grid) (* max-row max-col)) t
      (time-to-sync (flash (increment grid)) (+ t 1))))

;;Read input
(define input-file (open-input-file "Input11.txt"))
(define input (read-lines input-file))
(close-input-port input-file)
(define max-row (length input))
(define max-col (length (car input)))

;;Run simulations and display results
(display "Part 1: ")
(count-all-flashes input 0 100)
(display "Part 2: ")
(time-to-sync input 0)