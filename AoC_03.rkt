;;;AoC_03.rkt
;;;2021-12-03
;;;Mike Quigley

;;;Today we're decoding a diagnostic report for the submarine
;;;It involves manipulating binary numbers, and finding the most common and
;;;least common values in each position.

;;;The numbers read from the input are decimal with no leading 0s.
;;;Not the best way to represent binary.
;;;The intermediate values I create are lists of 0s and 1s, in reverse order
;;;(least significant bit first) for more efficient conversion to decimal

;;;Part 2 is about filtering, which is something this language is good at

#lang sicp
;Generates a list from a text file.
;Any whitespace in the file will act as a separator
(define (read-list file)
  (let ((line (read file)))
    (if (eof-object? line)
        nil
        (cons line (read-list file)))))

;Generate a list from 0 to n
(define (pos-seq from to)
  (if (> from to) nil
      (cons from (pos-seq (+ from 1) to))))

;Extracts a single digit from a binary number n
;pos is zero-based, right to left
(define (bit-at n pos)
  (modulo (floor (/ n (expt 10 pos))) 2))

;Converts a binary value from decimal to list representation
(define (bin-list val width)
  (map (lambda (x) (bit-at val x)) (pos-seq 0 width)))

;Finds most or least common bit. Determined by oper.
;Counts total number of ones and zeros. Returns 1 if ones oper zeros, else 0
(define (common-bit l pos oper)
  (define (iter l pos ones zeros)
    (cond ((null? l) (if (oper ones zeros) 1 0))
          ((= 1 (bit-at (car l) pos)) (iter (cdr l) pos (+ 1 ones) zeros))
          (else (iter (cdr l) pos ones (+ 1 zeros)))))
  (iter l pos 0 0))

;Finds most common bit. Returns 1 on ties
(define (most-common-bit l pos)
  (common-bit l pos >=))

;Finds least common bit. Returns 0 on ties
(define (least-common-bit l pos)
  (common-bit l pos <))

;Gamma rate. Returns value in the form of a list of length n
(define (gamma l n)
  (map (lambda (x) (most-common-bit l x)) (pos-seq 0 n)))

;Epsilon rate
(define (epsilon l n)
  (map (lambda (x) (least-common-bit l x)) (pos-seq 0 n)))

;Convert binary list to decimal.
(define (decimal b)
  (define (iter acc pos b-sub)
    (if (null? (cdr b-sub))
        (+ acc (* pos (car b-sub)))
        (iter (+ acc (* pos (car b-sub))) (* 2 pos) (cdr b-sub))))
  (iter 0 1 b))

;Filters a list l based on the bit-at function
;Returns the subset of l where bit-at pos equals val
(define (filter l val pos)
  (cond ((null? l) nil)
        ((= val (bit-at (car l) pos))
         (cons (car l) (filter (cdr l) val pos)))
        (else (filter (cdr l) val pos))))

(define (ls-rating criteria l pos)
  (let ((values (filter l (criteria l pos) pos)))
    (cond ((null? (cdr values)) (car values))
          (else (ls-rating criteria values (- pos 1))))))

(define (oxygen l width)
  (ls-rating most-common-bit l width))

(define (co2 l width)
  (ls-rating least-common-bit l width))

;;Entry Point ==================================================================
        
(define filename "Input3.txt")
(define bit-width 11)

(define input-file (open-input-file filename))
(define input (read-list input-file))
(close-input-port input-file)

(display "Part 1:")
(newline)

(define d-gamma (decimal (gamma input bit-width)))
(define d-epsilon (decimal (epsilon input bit-width)))

(display "Gamma:   ")
d-gamma
(display "Epsilon: ")
d-epsilon
(display "Product: ")
(* d-gamma d-epsilon)
(newline)

(display "Part 2:")
(newline)

(define d-oxygen (decimal (bin-list (oxygen input bit-width) bit-width)))
(define d-co2 (decimal (bin-list (co2 input bit-width) bit-width)))

(display "Oxygen:  ")
d-oxygen
(display "CO2:     ")
d-co2
(display "Product: ")
(* d-oxygen d-co2)
