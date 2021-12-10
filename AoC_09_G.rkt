;;;AoC_09_G.rkt
;;;2021-12-09
;;;Mike Quigley

;;;This is a modified version of AoC_09 that adds some graphics

#lang racket
(require graphics/graphics)
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

;Gets a value from the input heightmap. Returns 9 for invalid coords
(define (get-height row column)
  (cond ((and (>= row 0)
              (>= column 0)
              (< row max-row)
              (< column max-col))
         (list-ref (list-ref input row) column))
        (else 9)))

;Gets a list of all heights adjacent to coords
(define (get-adjacent-heights row column)
  (list (get-height (- row 1) column)
        (get-height (+ row 1) column)
        (get-height row (- column 1))
        (get-height row (+ column 1))))

;Do the given coords represent a low point?
(define (low-point? row column)
  (andmap (λ (x) (< (get-height row column) x))
          (get-adjacent-heights row column)))

;Gets the total risk level of a row
(define (total-risk row)
  (foldl (λ (x acc) (+ (if (low-point? row x)
                           (+ (get-height row x) 1)
                           0) acc))
         0
         (range 0 max-col)))

;Makes a list of all low points, represented by (row . column) pairs
(define (list-lows)
  (define (list-low-row row)
    (foldl (λ (x acc) (if (low-point? row x) (cons (cons row x) acc) acc))
           null (range 0 max-col)))
  (foldl append null (map list-low-row (range 0 max-row))))

;Given the coords of a low point, return the set of all points in the basin
;direction is used to prevent infinite loops between 2 points of equal height
(define (basin-points direction prev-height row column)
  (let ((current-height (get-height row column)))
    (cond ((= current-height 9) (set))
          ((< current-height prev-height) (set))
          (else (draw-box row column)
                (sleep 0.0005)
                (set-union
                 (set (cons row column))
                 (if (eq? direction 'North)
                     (set)
                     (basin-points 'South current-height (+ row 1) column))
                 (if (eq? direction 'South)
                     (set)
                     (basin-points 'North current-height (- row 1) column))
                 (if (eq? direction 'East)
                     (set)
                     (basin-points 'West current-height row (- column 1)))
                 (if (eq? direction 'West)
                     (set)
                     (basin-points 'East current-height row (+ column 1))))))))

;Finds the size of the basin for the given low point
;note that this takes the point as a (row . column) pair
(define (basin-size point)
  (set-count (basin-points 'Low 0 (car point) (cdr point))))

(define (draw-box row column)
  ((draw-solid-rectangle window)
   (make-posn (* column 5) (* row 5)) 5 5
   (make-rgb 0 (/ (get-height row column) 10) 0)))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input09.txt"))
(define input (read-lines input-file))
(close-input-port input-file)
(define max-row (length input))
(define max-col (length (car input)))

(display "Part 1: ")
(foldl (λ (x acc) (+ acc (total-risk x))) 0 (range 0 max-row))
(display "Part 2: ")
(open-graphics)
(define window (open-viewport "Cavern Map" 500 500))
((draw-viewport window) "black")
(define basins (sort (map basin-size (list-lows)) >))
(* (car basins) (cadr basins) (caddr basins))