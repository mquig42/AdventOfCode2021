;;;AoC_06.rkt
;;;2021-12-06
;;;Mike Quigley

;;;We're simulating lanternfish reproduction now. This isn't really a cellular
;;;automaton, since each fish is represented by a countdown timer.
;;;I notice that part 1 doesn't say anything about the lifespan of a fish.
;;;Maybe part 2 simulates that.
;;;Nope, part 2 continues to assume fish are immortal and just increases
;;;the number of days from part 1. Unfortunately, my first algorithm
;;;wasn't fast enough to deal with 256 days, so I had to write a better one.

#lang racket

;Count the number of times n appears in lst
(define (count-n lst n)
  (define (iter acc lst)
    (cond ((null? lst) acc)
          ((= (car lst) n) (iter (+ acc 1) (cdr lst)))
          (else (iter acc (cdr lst)))))
  (iter 0 lst))

;Replace -1 with 6
(define (replace-1 x)
  (if (= x -1) 6 x))

;Generate a list of 8s of length n
(define (seq-8 n)
  (if (= 0 n) null
      (cons 8 (seq-8 (- n 1)))))

;Increment day.
;The process for each day is:
;Decrement each fish by 1, then count the number of -1s in the list,
;then replace any -1s with 6s and add a number of 8s to the end of the list.
(define (inc-day fishes)
  (let ((decremented (map (λ (x) (- x 1)) fishes)))
    (append (map replace-1 decremented) (seq-8 (count-n decremented -1)))))

(define (inc-n-days fishes n)
  (if (= n 0) fishes
      (inc-n-days (inc-day fishes) (- n 1))))

;More efficient method. Instead of a list of each fish, use a summary
;The position in the list represents the timer, and the number represents
;the number of fish with that timer value.
;eg. (3 4 3 1 2) would be (0 1 1 2 1 0 0 0 0)
(define (inc-day-two fishes)
  (let ((eights (car fishes)))
    (list (second fishes)
          (third fishes)
          (fourth fishes)
          (fifth fishes)
          (sixth fishes)
          (seventh fishes)
          (+ (eighth fishes) eights)
          (ninth fishes)
          eights)))

(define (inc-n-days-two fishes n)
  (if (= n 0) fishes
      (inc-n-days-two (inc-day-two fishes) (- n 1))))

;Converts input list into format for part 2 algorithms
(define (convert-input input)
  (map (λ (x) (count-n input x)) (range 9)))

;Calculate the sum of all items in lst
(define (sum lst)
  (define (iter acc lst)
    (if (null? lst) acc
        (iter (+ acc (car lst)) (cdr lst))))
  (iter 0 lst))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input06.txt"))
(define input (map string->number
                   (string-split (string-trim (read-line input-file)) ",")))
(close-input-port input-file)

(display "Part 1: ")
(length (inc-n-days input 80))
(display "Part 2: ")
(sum (inc-n-days-two (convert-input input) 256))
