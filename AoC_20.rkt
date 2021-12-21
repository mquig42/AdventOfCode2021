;;;AoC_20.rkt
;;;2021-12-20
;;;Mike Quigley

;;;This seems much more straightforward than yesterday. May be best to store
;;;the image as a hash set of coordinates, rather than a 2D list.
;;;Finding extents would take linear time, but all lookups would be constant.
;;;Don't convert from that into a list of strings to display the final image.
;;;I did that on day 13, and it's slow for large images.
;;;The graphics library from day 9 would be a better choice.

;;;One tricky thing is that, in my input, the first char of the "algorithm"
;;;is # and the last is .
;;;This means that the background colour swaps on each round of enhancement
;;;Need to take that into account instead of just assuming it's always dark.

;;;Part 1 asks for two rounds. The number of lit pixels is only infinite
;;;if the number of rounds is odd.
;;;Part 2 is just part 1 with 50 rounds of enhancement instead of 2. Luckily
;;;my data structures and algorithms were good enough and this only took
;;;20 seconds to run.

#lang racket
(require graphics/graphics)

;Reads the input file and generates a set of lit coordinates
(define (read-input-grid file)
  (define (read-points lst coord-set row column)
    (cond ((null? lst) coord-set)
          ((eq? (car lst) #\#)
           (read-points (cdr lst)
                        (set-add coord-set (cons row column))
                        row
                        (+ column 1)))
          (else (read-points (cdr lst) coord-set row (+ column 1)))))
  (define (read-row file coord-set row)
    (let ((str (read-line file)))
      (if (eof-object? str) coord-set
          (read-row file
                    (read-points (string->list (string-trim str))
                                 coord-set row 0)
                    (+ row 1)))))
  (read-row file (set) 0))

;Converts a binary list of arbitrary length to a decimal number
(define (bin->dec bin-lst)
  (foldl (λ (x acc) (+ (* acc 2) x)) 0 bin-lst))

;;Extent list functions
;Finds the extent list of given coords
(define (get-extents coord-set)
  (define (inc-extent coord extent-list)
    (list (min (min-row extent-list) (car coord))
          (min (min-col extent-list) (cdr coord))
          (max (max-row extent-list) (car coord))
          (max (max-col extent-list) (cdr coord))))
  (foldl inc-extent '(0 0 0 0) (set->list coord-set)))

;Getters for extent list
(define (min-row extents)
  (car extents))
(define (min-col extents)
  (cadr extents))
(define (max-row extents)
  (caddr extents))
(define (max-col extents)
  (cadddr extents))

;Expands the extent list by 1 in each direction
(define (expand-extents extents)
  (list (- (min-row extents) 1)
        (- (min-col extents) 1)
        (+ (max-row extents) 1)
        (+ (max-col extents) 1)))

(define (enhance coord-set extents extents-nxt background)
  ;Returns 1 if the given coord is lit in coord-set, 0 if it isn't
  (define (get-coord-value row col)
    (cond ((or (< row (min-row extents))
               (< col (min-col extents))
               (> row (max-row extents))
               (> col (max-col extents))) background)
          ((set-member? coord-set (cons row col)) 1)
          (else 0)))

  ;Returns true if the given coords should be lit after enhancing coord-set
  (define (lit? row col)
    (vector-ref algorithm (bin->dec
                           (list (get-coord-value (- row 1) (- col 1))
                                 (get-coord-value (- row 1) col)
                                 (get-coord-value (- row 1) (+ col 1))
                                 (get-coord-value row (- col 1))
                                 (get-coord-value row col)
                                 (get-coord-value row (+ col 1))
                                 (get-coord-value (+ row 1) (- col 1))
                                 (get-coord-value (+ row 1) col)
                                 (get-coord-value (+ row 1) (+ col 1))))))

  (define (iter row col result-grid)
    (cond ((> row (max-row extents-nxt)) result-grid)
          ((> col (max-col extents-nxt)) (iter (+ row 1)
                                               (min-col extents-nxt)
                                               result-grid))
          (else (iter row (+ col 1)
                      (if (lit? row col) (set-add result-grid (cons row col))
                          result-grid)))))
  (iter (min-row extents-nxt) (min-col extents-nxt) (set)))

(define (enhance-n n coord-set extents background)
  (let ((extents-nxt (expand-extents extents)))
    (if (= n 0) coord-set
        (enhance-n (- n 1)
                   (enhance coord-set extents extents-nxt background)
                   extents-nxt
                   (if (= background 1) 0 odd-background)))))

;Draws the image
(define (draw coord-set)
  (let ((extents (get-extents coord-set)))
    (define window
      (open-viewport "Image"
                     (+ (- (max-row extents) (min-row extents)) 1)
                     (+ (- (max-col extents) (min-col extents)) 1)))
    ((draw-viewport window) "black")
    (for-each (λ (x) ((draw-pixel window)
                      (make-posn (- (car x) (min-row extents))
                                 (- (cdr x) (min-col extents)))
                      "green"))
              (set->list coord-set))))

;;Read input
(define input-file (open-input-file "Input20.txt"))
;We'll be making a lot of lookups into this, so make it an immutable vector
;instead of a list for constant-time access.
(define algorithm
  (vector->immutable-vector
   (list->vector (map (λ (x) (if (eq? x #\#) true false))
                      (string->list (string-trim (read-line input-file)))))))
(void (read-line input-file)) ;Skip blank line between algorithm and grid
(define input-grid (read-input-grid input-file))
(close-input-port input-file)

;If the first char of the algorithm is #, then the background will alternate
;between dark and light. If it's . then the background will always be dark
(define odd-background (if (vector-ref algorithm 0) 1 0))
(define input-extents (get-extents input-grid))

(display "Part 1: ")
(set-count (enhance-n 2 input-grid input-extents 0))
(display "Part 2: ")
(define full-image (enhance-n 50 input-grid input-extents 0))
(set-count full-image)
(open-graphics)
(draw full-image)
