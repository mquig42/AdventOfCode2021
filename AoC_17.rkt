;;;AoC_17.rkt
;;;2021-12-17
;;;Mike Quigley

;;;Ballistics calculations? Going back to the beginning I see.
;;;After writing a basic simulation that takes a starting velocity
;;;in x and y components, run it to find 2 things:
;;;Part 1: The maximum y-velocity that can hit the target area
;;;Part 2: The number of different velocities that can hit the target area

;;;Functional programming! Enumerate, map, and filter made part 2 easy.

;;;Need to rewrite part 1, it doesn't work if there's a discontinuity.

#lang racket
;Converts an input string into a list of 4 values, representing the minimum
;and maximum x and y coordinates
(define (decode-input str)
  (let* ((str1 (substring str 13))
         (xy (string-split str1 ","))
         (x-range (substring (car xy) 2))
         (y-range (substring (cadr xy) 3)))
    (list (string->number (car (string-split x-range "..")))
          (string->number (cadr (string-split x-range "..")))
          (string->number (car (string-split y-range "..")))
          (string->number (cadr (string-split y-range ".."))))))

;;Getters for target
(define (x-min tgt)
  (car tgt))
(define (x-max tgt)
  (cadr tgt))
(define (y-min tgt)
  (caddr tgt))
(define (y-max tgt)
  (cadddr tgt))

;;Getters for probe state (position and velocity)
(define (x-pos probe)
  (car probe))
(define (y-pos probe)
  (cadr probe))
(define (x-vel probe)
  (caddr probe))
(define (y-vel probe)
  (cadddr probe))
(define (probe-state x-pos y-pos x-vel y-vel)
  (list x-pos y-pos x-vel y-vel))

;Increments a probe's state by 1 step
(define (step probe)
  (probe-state (+ (x-pos probe) (x-vel probe))
               (+ (y-pos probe) (y-vel probe))
               (max (- (x-vel probe) 1) 0)
               (- (y-vel probe) 1)))

;Inits a probe with given starting velocity
(define (launch x-vel y-vel)
  (probe-state 0 0 x-vel y-vel))

;Returns true if probe is within target area
(define (hit? probe target)
  (and (>= (x-pos probe) (x-min target))
       (<= (x-pos probe) (x-max target))
       (>= (y-pos probe) (y-min target))
       (<= (y-pos probe) (y-max target))))

;Returns true if probe has passed the target area
(define (miss? probe target)
  (or (> (x-pos probe) (x-max target))
      (< (y-pos probe) (y-min target))))

;Runs simulation until probe has either hit or passed target area
;Returns true for hit, false for miss
(define (will-hit? probe target)
  (cond ((hit? probe target) true)
        ((miss? probe target) false)
        (else (will-hit? (step probe) target))))

;Runs simulation with given y-vel and a range of x-vels
;Returns true if it is possible for a probe with given y-vel to hit target
(define (will-hit-y? y-vel target)
  (< 0 (length
        (filter identity
                (map (λ (x) (will-hit? (launch x y-vel) target))
                     (range (x-max target)))))))

;Attempts to find highest y-vel that can hit target
;Doesn't work. For distant targets, there will be a gap between
;direct-fire (high x-vel, low y-vel) and indirect-fire (high y, low x)
(define (fastest-y target)
  (define (iter y-vel)
    (if (will-hit-y? y-vel target) (iter (+ y-vel 1)) (- y-vel 1)))
  (iter 0))

;Finds the maximum height of any trajectory with the given starting y-vel
(define (max-height y-vel)
  (/ (* y-vel (+ y-vel 1)) 2))

;Need to enumerate a bunch of starting (x-vel . y-vel) pairs
;The actual highest y-vel for my input is 89
;(found by mapping will-hit-y? over a range)
(define (enumerate-velocities x-vel-min x-vel-max y-vel-min y-vel-max)
  (define (iter x-vel y-vel)
    (cond ((> x-vel x-vel-max) null)
          ((= y-vel y-vel-max)
           (cons (cons x-vel y-vel) (iter (+ x-vel 1) y-vel-min)))
          (else (cons (cons x-vel y-vel) (iter x-vel (+ y-vel 1))))))
  (iter x-vel-min y-vel-min))

(define (filter-velocities lst target)
  (filter identity
          (map (λ (vels) (will-hit? (launch (car vels) (cdr vels)) target))
               lst)))

(define input-file (open-input-file "Input17.txt"))
(define input (string-trim (read-line input-file)))
(close-input-port input-file)

;(define target (list 20 30 -10 -5))
(define target (decode-input input))

(display "Part 1: ")
(max-height (fastest-y target))
(display "Part 2: ")
(length
 (filter-velocities
  (enumerate-velocities 1 (x-max target) (y-min target) 100)
  target))