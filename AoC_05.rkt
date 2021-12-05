;;;AoC_05.rkt
;;;2021-12-05
;;;Mike Quigley

;;;Today we're navigating the ocean floor, trying to avoid hydrothermal vents
;;;The input contains a bunch of line segments. Find the number of points where
;;;those segments intersect. The only difference between parts 1 and 2
;;;is whether to consider diagonal lines.

;;;The best way to solve this is with hash tables. This looks like a good
;;;opportunity to learn how those work in Racket.
;;;It can also be solved (slowly) using the member and remove* functions

#lang racket

;Reads a single line segment from input file
(define (read-segment file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (map string->number
             (list (car (string-split line ","))
                   (cadr (string-split (car (string-split line " -> ")) ","))
                   (car (string-split (cadr (string-split line " -> ")) ","))
                   (string-trim (caddr (string-split line ","))))))))

;Make a list of every line segment in the file
(define (read-all-segments file)
  (let ((segment (read-segment file)))
    (if (null? segment) null
        (cons segment (read-all-segments file)))))

;;Getters for segments
(define (x1 segment)
  (car segment))
(define (y1 segment)
  (cadr segment))
(define (x2 segment)
  (caddr segment))
(define (y2 segment)
  (cadddr segment))
(define (x-min segment)
  (min (x1 segment) (x2 segment)))
(define (x-max segment)
  (max (x1 segment) (x2 segment)))
(define (y-min segment)
  (min (y1 segment) (y2 segment)))
(define (y-max segment)
  (max (y1 segment) (y2 segment)))
(define (y-left segment)
  (if (> (x2 segment) (x1 segment)) (y1 segment) (y2 segment)))

;;Getters and setters for points
(define (x-coord point)
  (car point))
(define (y-coord point)
  (cdr point))
(define (mk-point x y)
  (cons x y))

;;Determine direction of line segment.
;;A diagonal segment is upward if x and y both increase along its length,
;;or downward if y decreases as x increases
(define (horizontal? segment)
  (= (y1 segment) (y2 segment)))
(define (vertical? segment)
  (= (x1 segment) (x2 segment)))
(define (upward? segment)
  (or (and (> (x2 segment) (x1 segment)) (> (y2 segment) (y1 segment)))
      (and (< (x2 segment) (x1 segment)) (< (y2 segment) (y1 segment)))))
(define (downward? segment)
  (or (and (> (x2 segment) (x1 segment)) (< (y2 segment) (y1 segment)))
      (and (< (x2 segment) (x1 segment)) (> (y2 segment) (y1 segment)))))

;For part 1, we don't want to consider diagonal segments. Remove them.
(define (remove-diagonals lst)
  (filter (λ (x) (or (horizontal? x) (vertical? x))) lst))

;Makes a list of all points that a given line segment passes through.
(define (enumerate-points segment)
  (define (iter point get-coord x-add y-add max)
    (if (> (get-coord point) (max segment)) null
        (cons point
              (iter (mk-point (+ (x-coord point) x-add)
                                (+ (y-coord point) y-add))
                    get-coord x-add y-add max))))
  (cond ((horizontal? segment)
         (iter (mk-point (x-min segment) (y1 segment)) x-coord 1 0 x-max))
        ((vertical? segment)
         (iter (mk-point (x1 segment) (y-min segment)) y-coord 0 1 y-max))
        ((upward? segment)
         (iter (mk-point (x-min segment) (y-left segment)) x-coord 1 1 x-max))
        ((downward? segment)
         (iter (mk-point (x-min segment) (y-left segment)) x-coord 1 -1 x-max))
        (else null)))

;Perform a single level of flattening on a list of lists
(define (flatten-one lst)
  (if (null? lst) null
      (append (car lst) (flatten-one (cdr lst)))))

;Counts the number of items which appear more than once in lst
;This relies on linear operations for searching and filtering lst, so it's slow
(define (count-duplicates lst)
  (define (iter acc lst)
    (cond ((null? lst) acc)
          ((member (car lst) (cdr lst))
           (iter (+ acc 1) (remove* (list (car lst)) lst)))
          (else (iter acc (cdr lst)))))
  (iter 0 lst))

;New implememtation using hash sets.
;This is (as expected) much faster. It takes 3 seconds to solve part 2,
;while count-duplicates takes over 13 minutes
(define (count-duplicates-fast lst)
  (define (iter dups points lst)
    (cond ((null? lst) (set-count dups)) ;Reached end of lst, return count
          ((set-member? dups (car lst))  ;This duplicate has already been found
           (iter dups points (cdr lst)))
          ((set-member? points (car lst));Found a duplicate, add it to dups
           (iter (set-add dups (car lst)) points (cdr lst)))
          (else                          ;Not a duplicate, add it to points
           (iter dups (set-add points (car lst)) (cdr lst)))))
  (iter (set) (set) lst))

;Solve part 1
(define (solve1 input)
  (count-duplicates-fast
   (flatten-one
    (map enumerate-points
         (remove-diagonals input)))))

;Solve part 2. Same as part 1, but don't remove diagonals
(define (solve2 input)
  (count-duplicates-fast (flatten-one (map enumerate-points input))))

;;Entry Point ==================================================================
(define filename "Input5.txt")
(define input-file (open-input-file filename))
(define input (read-all-segments input-file))
(close-input-port input-file)

(display "Part 1: ")
(solve1 input)
(display "Part 2: ")
(solve2 input)
          
