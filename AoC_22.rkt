;;;AoC_22.rkt
;;;2021-12-22
;;;Mike Quigley

;;;Based on reading the description, all I have to do here is set a bunch of
;;;voxels on or off based on instructions in the input.

;;;It's likely that a strategy of tracking individual voxels would work for
;;;part 1 but be too slow or memory-intensive for part 2
;;;A single randomly-chosen line of my input turns on 13,111,026,714,912 voxels
;;;That's too many to track individually.

;;;The right answer is probably this:
;;;Keep a list of all "on" cuboids. For each new cuboid from input, take
;;;any existing ones that intersect and replace them with several smaller ones
;;;that don't. If the new cuboid is "on", add it to the list.
;;;Figuring out the splitting could be tricky.
;;;How about this: Keep a list of cuboids that are either "on" or "off"
;;;Same format as the parsed instructions.
;;;When adding a new cuboid from instructions, find the volumes where it
;;;overlaps existing cuboids and insert new "on" or "off" cuboids to represent
;;;the overlapping volumes.
;;;Find the total value of all "on" cuboids and subtract the total volume of
;;;the "off" cuboids

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

;Finds the intersection of two cuboids. If they do not overlap,
;return '(0 -1 0 -1 0 -1)
(define (intersect box-a box-b)
  (let ((intersection (list (max (x-min box-a) (x-min box-b))
                            (min (x-max box-a) (x-max box-b))
                            (max (y-min box-a) (y-min box-b))
                            (min (y-max box-a) (y-max box-b))
                            (max (z-min box-a) (z-min box-b))
                            (min (z-max box-a) (z-max box-b)))))
    (if (and (> (x-max intersection) (x-min intersection))
             (> (y-max intersection) (y-min intersection))
             (> (z-max intersection) (z-min intersection)))
        intersection
        '(0 -1 0 -1 0 -1))))

;Finds the volume of a cuboid
(define (volume box-coords)
  (* (- (x-max box-coords) (x-min box-coords) -1)
     (- (y-max box-coords) (y-min box-coords) -1)
     (- (z-max box-coords) (z-min box-coords) -1)))

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
(define (solve1 input boundary)
  (foldl (λ (x acc) (if (eq? 'on (car x))
                        (set-union
                         acc
                         (enumerate-voxels (intersect (cdr x) boundary)))
                        (set-subtract
                         acc
                         (enumerate-voxels (intersect (cdr x) boundary)))))
         (set)
         input))

;Based on overlapping positive and negative cuboids,
(define (solve2 input)
  (define (intersect-list box-coords lst acc)
    (cond ((null? lst) acc)
          ((equal? (intersect box-coords (car lst)) '(0 -1 0 -1 0 -1))
           (intersect-list
            box-coords (cdr lst) acc))
          (else
           (intersect-list
            box-coords
            (cdr lst)
            (cons (intersect box-coords (car lst)) acc)))))
  (define (iter input on off)
    (let ((instruction (if (null? input) null (car input))))
      (cond ((null? input)
             (- (foldl + 0 (map volume on)) (foldl + 0 (map volume off))))
            ((eq? 'on (car instruction))
             (iter (cdr input)
                   (cons (cdr instruction)
                         (append (intersect-list (cdr instruction) off null)
                                 on))
                   (append (intersect-list (cdr instruction) on null) off)))
            ((eq? 'off (car instruction))
             (iter (cdr input)
                   (append (intersect-list (cdr instruction) off null) on)
                   (append (intersect-list (cdr instruction) on null) off))))))
  (iter input null null))

;;Read input
(define input-file (open-input-file "Input22.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(define boundary '(-50 50 -50 50 -50 50))

;;Display output
(display "Part 1: ")
(set-count (solve1 input boundary))
(display "Part 2: ")
(solve2 input)
