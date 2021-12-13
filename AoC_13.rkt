;;;AoC_13.rkt
;;;2021-12-03
;;;Mike Quigley

;;;Today's input is in two sections. The first is a set of coordinates
;;;representing dots on a large transparency. The second is instructions
;;;for folding that transparency. Once it's folded, the dots should form
;;;an image.

;;;To solve this, make a function that applies the folding operation
;;;to a single set of coordinates. For example, (1 . 10) folded by y=7
;;;transforms into (1 . 4). Then just map that over the entire list of coords.
;;;There will be duplicates, so use the remove-duplicates function.

;;;Once all the folding has been done, need to transform the set of
;;;coordinates into a 2D representation of the value of each point
;;;A list of strings should work and be easy to display.

#lang racket
;Reads the first section of the input into a list of coord pairs
(define (read-coords file)
  (let ((line (read-line file)))
    (if (string=? "\r" line) null
        (let ((coords (map string->number
                           (string-split (string-trim line) ","))))
          (cons (cons (car coords) (cadr coords)) (read-coords file))))))

;Reads the instructions from the second section of the input file
(define (read-instructions file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (decode-instruction (caddr (string-split (string-trim line) " ")))
              (read-instructions file)))))

;Transform a folding instruction string from the input file into a pair
;For example, "y=7" turns into ("y" . 7)
(define (decode-instruction instruction)
  (let ((sp (string-split instruction "=")))
    (cons (car sp) (string->number (cadr sp)))))

;;Folding functions
;Applies the folding operation to a single number
;(one dimension of a coord pair). Point is the position of the dot,
;line is the position of the fold line
(define (fold-dim point line)
  (if (< point line) point
      (- line (- point line))))

;Applies the folding operation to a coord pair
(define (fold-coord coord instr)
  (if (string=? "x" (car instr))
      (cons (fold-dim (car coord) (cdr instr)) (cdr coord))
      (cons (car coord) (fold-dim (cdr coord) (cdr instr)))))

;Applies one fold operation to the entire sheet
(define (fold-sheet sheet instr)
  (remove-duplicates
   (map (λ (x) (fold-coord x instr)) sheet)))

;Applies all fold operations to the entire sheet
(define (fold-all)
  (foldl (λ (x acc) (fold-sheet acc x)) coords instructions))

(define (find-extent coords)
  (foldl (λ (x acc)
           (cons (max (car x) (car acc)) (max (cdr x) (cdr acc))))
         '(0 . 0)
         coords))

;;Transform coords into grid for display
(define (generate-line coords line max-x)
  (list->string
   (map (λ (x) (if (member (cons x line) coords) #\▓ #\space)) (range max-x))))

(define (generate-grid coords max-x max-y)
  (map (λ (x) (generate-line coords x max-x)) (range max-y)))

;;Read input
(define input-file (open-input-file "Input13.txt"))
(define coords (read-coords input-file))
(define instructions (read-instructions input-file))
(close-input-port input-file)

(display "Part 1: ")
(length (fold-sheet coords (car instructions)))

(newline)
(display "Part 2: ")
(newline)
(define final-coords (fold-all))
(define extent (find-extent final-coords))
(generate-grid final-coords (+ 1 (car extent)) (+ 1 (cdr extent)))