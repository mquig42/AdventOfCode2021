;;;AoC_22.rkt
;;;2021-12-22
;;;Mike Quigley

;;;Based on reading the description, all I have to do here is set a bunch of
;;;voxels on or off based on instructions in the input.

;;;Once I create a function that enumerates all the voxels in a 3D range,
;;;making sure to do bounds checking, the rest should take care of itself.
;;;Just add or remove them from a set and do a set-count at the end.
;;;I notice, however, that the first 20 lines of input have small numbers,
;;;and the remaining 400 are much larger but out of bounds for part 1

;;;It's likely that a strategy of tracking individual voxels would work for
;;;part 1 but be too slow or memory-intensive for part 2
;;;A single randomly-chosen line of my input turns on 13,111,026,714,912 voxels
;;;That's too many to track individually.

;;;This sort of adding and subtracting cubes is something CAD programs do.
;;;How do they work?
;;;https://en.wikipedia.org/wiki/Constructive_solid_geometry

;;;As I suspected, part 2 is just part 1 without boundaries
;;;The volume of the entire area is 6829 times bigger than the part 1 area
;;;Since my voxel-counting algorithm takes 13 seconds to solve part 1, I can
;;;just subdivide the volume into 100x100x100 regions and take the sum of
;;;each one. This should take approximately 24 hours to run. Make that plan B.

;;;Terminology: I will be using "coords" to mean a single point in 3D space,
;;;and "box-coords" to represent a range of x, y and z values representing
;;;a cuboid.

#lang racket
;Parse 1 line of input into a list of 7 elements
;First is 'on or 'off, remaining 6 are minimum and maximum for x, y, and z
(define (parse-line str)
  (let ((mins (regexp-match* #px"(?<==)-?\\d+" str))
        (maxes (regexp-match* #px"(?<=\\.)-?\\d+" str)))
    (cons (read (open-input-string str))
          (map string->number
               (list (car mins) (car maxes)
                     (cadr mins) (cadr maxes)
                     (caddr mins) (caddr maxes))))))

;Reads and parses all input from file
(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (parse-line line) (read-input file)))))

;Finds a bounding box that includes all points in input
(define (get-extents input)
  (foldl (λ (x acc) (list (min (x-min (cdr x)) (x-min acc))
                          (max (x-max (cdr x)) (x-max acc))
                          (min (y-min (cdr x)) (y-min acc))
                          (max (y-max (cdr x)) (y-max acc))
                          (min (z-min (cdr x)) (z-min acc))
                          (max (z-max (cdr x)) (z-max acc))))
         '(0 0 0 0 0 0)
         input))

;;Getters
(define (x-min box-coords)
  (first box-coords))
(define (x-max box-coords)
  (second box-coords))
(define (y-min box-coords)
  (third box-coords))
(define (y-max box-coords)
  (fourth box-coords))
(define (z-min box-coords)
  (fifth box-coords))
(define (z-max box-coords)
  (sixth box-coords))

;range limiting function. Returns the section of box-coords that is within
;the boundary. If it is completely outside, then at least one of the
;minimum values in the returned list will be greater than the maximum
(define (bounded-range box-coords boundary)
  (list (max (x-min box-coords) (x-min boundary))
        (min (x-max box-coords) (x-max boundary))
        (max (y-min box-coords) (y-min boundary))
        (min (y-max box-coords) (y-max boundary))
        (max (z-min box-coords) (z-min boundary))
        (min (z-max box-coords) (z-max boundary))))

;Returns a set of all voxels within given box-coords
;I know this won't work for part 2, but I'd like to find out what exactly
;part 2 is
(define (enumerate-voxels box-coords)
  (define (iter x y z acc)
    (cond ((> z (z-max box-coords)) acc)
          ((> y (y-max box-coords)) (iter (x-min box-coords)
                                          (y-min box-coords)
                                          (+ z 1)
                                          acc))
          ((> x (x-max box-coords)) (iter (x-min box-coords)
                                          (+ y 1)
                                          z
                                          acc))
          (else (iter (+ x 1) y z (set-add acc (list x y z))))))
  (iter (x-min box-coords) (y-min box-coords) (z-min box-coords) (set)))

;Returns the set of all voxels that are turned on after following
;the instructions in input
(define (make-all-boxes input boundary)
  (foldl (λ (x acc) (if (eq? 'on (car x))
                        (set-union
                         acc
                         (enumerate-voxels (bounded-range (cdr x) boundary)))
                        (set-subtract
                         acc
                         (enumerate-voxels (bounded-range (cdr x) boundary)))))
         (set)
         input))

;;Read input
(define input-file (open-input-file "Input22.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(define boundary '(-50 50 -50 50 -50 50))

;;Display output
(display "Part 1: ")
(set-count (make-all-boxes input boundary))
