;;;AoC_07.rkt
;;;2021-12-07
;;;Mike Quigley

;;;Optimization problem. Given a list of submarines in different positions,
;;;find the minimum fuel required to move them all to the same position
;;;using two different methods of calculating fuel consumption

;;;Since the only difference between parts one and two are the different
;;;movement-cost functions, it was easy to refactor.
;;;I'm starting to like functional programming.

;;;Part 2 is now optimized. It used to do the calculation iteratively,
;;;which took over 30 seconds to run. Thanks to Gauss, that's down to 175ms.

#lang racket
;Calculates the fuel required to move all the submarines in lst to position dest
(define (total-movement-cost lst dest cost)
  (foldl (λ (pos acc) (+ acc (cost pos dest))) 0 lst))

;Calculates total movement cost for every possible destination, returns minimum
(define (solve lst cost)
  (foldl (λ (x acc) (min (total-movement-cost lst x cost) acc))
         99999999
         (range (argmin identity lst) (+ 1 (argmax identity lst)))))

;Movement cost calculation for part 1
(define (movement-cost-1 from to)
  (abs (- from to)))

;Movement cost for part 2
(define (movement-cost-2 from to)
  (let ((dist (+ 1 (abs (- from to)))))
        (/ (* dist (- dist 1)) 2)))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input07.txt"))
(define input (map string->number
                   (string-split (string-trim (read-line input-file)) ",")))
(close-input-port input-file)

(display "Part 1: ")
(solve input movement-cost-1)
(display "Part 2: ")
(solve input movement-cost-2)