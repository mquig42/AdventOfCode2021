;;;AoC_19.rkt
;;;2021-12-19
;;;Mike Quigley

;;;So, this is like the sea monster problem from last year, only even *more*
;;;complicated. Instead of every tile having unique and easily matchable edges,
;;;this year they overlap, so I need to match based on their actual content.

;;;Also, it's in 3D. This means the tiles can be rotated to 24 different
;;;orientations.

;;;I already spent much of today solving yesterday's puzzle, so finishing this
;;;may have to wait. Let's try to at least read the input.

;;;Done. NOTE: file must end with two newlines, not just one

;;;Next, need to find if 2 sensors have overlapping fields of view
;;;How about making a set of the distance between every combination of
;;;two beacons. These distances might not be completely unique, but
;;;it's probably unlikely for 12 of them to match

;;;There are 25-27 beacons in each sensor report
;;;This means 300-351 combinations

;;;Done. Now I need to merge two sensors. To do that, I need to rotate
;;;one of them to match the other one, then find the x, y, and z distances so
;;;I can move it into the correct position.

#lang racket
;Read a single scan from the input file
(define (read-scan file)
  (define (read-coords)
    (let ((str (string-trim (read-line file))))
      (cond ((string=? str "") null)
            (else
             (cons (map string->number (string-split str ","))
                   (read-coords))))))
  (if (eof-object? (read-line file)) null
      (read-coords)))

;Read all the scans from the input file
(define (read-scans file)
  (let ((scan (read-scan file)))
    (if (null? scan) null
        (cons scan (read-scans file)))))

;;Getters
(define (x-coord point)
  (car point))
(define (y-coord point)
  (cadr point))
(define (z-coord point)
  (caddr point))

;Calculates the distance between two points
(define (distance a b)
  (exact-round (sqrt (+ (sqr (- (x-coord a) (x-coord b)))
                        (sqr (- (y-coord a) (y-coord b)))
                        (sqr (- (z-coord a) (z-coord b)))))))

;Returns a set of the distances between every combination of two points in lst
(define (distance-set lst)
  (list->set (map (λ (i) (distance (car i) (cadr i)))
                  (combinations lst 2))))

;Given a scan report m and a list of other scan reports lst,
;find the report with the greatest number of distances in common with m
(define (find-match m lst)
  (argmax (λ (i) (set-count (set-intersect (distance-set m)
                                           (distance-set i))))
          lst))

;Merge two sensors
(define (merge a b)
  null)

;Merge all sensors and return full map
(define (merge-all m lst)
  (if (null? lst) m
      (let ((matching-sensor (find-match m lst)))
        (merge-all (merge m matching-sensor)
                   (remove matching-sensor lst)))))

(define input-file (open-input-file "Input19.txt"))
(define input (read-scans input-file))
(close-input-port input-file)
