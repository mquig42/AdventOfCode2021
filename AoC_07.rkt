;;;AoC_07.rkt
;;;2021-12-07
;;;Mike Quigley

;;;Optimization problem. Given a list of submarines in different positions,
;;;find the minimum fuel required to move them all to the same position
;;;using two different methods of calculating fuel consumption

;;;Since the only difference between parts one and two are the different
;;;movement-cost functions, it was easy to refactor.
;;;I'm starting to like functional programming.

;;;Right now my part 2 solution takes over 30 seconds to run. This could be
;;;improved by precomputing all the movement costs and making a lookup table.
;;;Also, didn't Euler come up with an algebraic solution? Or was it Gauss?
;;;That would be even better.

#lang racket
;Flexible function that can find the maximum or minimum value of a list
(define (list-val-helper initial comparison lst)
  (cond ((null? lst) initial)
        ((comparison (car lst) initial)
         (list-val-helper (car lst) comparison (cdr lst)))
        (else (list-val-helper initial comparison (cdr lst)))))

;Returns maximum value of a list
(define (max-val lst)
  (list-val-helper (car lst) > lst))

;Returns minimum value of a list
(define (min-val lst)
  (list-val-helper (car lst) < lst))

;Calculates the fuel required to move all the submarines in lst to position dest
(define (total-movement-cost lst dest cost)
  (define (iter acc lst)
    (if (null? lst) acc
        (iter (+ acc (cost (car lst) dest)) (cdr lst))))
  (iter 0 lst))

;Calculates total movement cost for every possible destination, returns minimum
(define (solve lst cost)
  (min-val (map (λ (x) (total-movement-cost lst x cost))
                (range (min-val lst) (+ 1 (max-val lst))))))

;Movement cost calculation for part 1
(define (movement-cost-1 from to)
  (abs (- from to)))

;Movement cost calculation for part 2
(define (movement-cost-2 from to)
  (define (iter acc distance)
    (if (= 0 distance) acc
        (iter (+ acc distance) (- distance 1))))
  (iter 0 (abs (- from to))))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input07.txt"))
(define input (map string->number
                   (string-split (string-trim (read-line input-file)) ",")))
(close-input-port input-file)

(display "Part 1: ")
(solve input movement-cost-1)
(display "Part 2: ")
(solve input movement-cost-2)