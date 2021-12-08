;;;AoC_08.rkt
;;;2021-12-08
;;;Mike Quigley

;;;Solve some scrambled signal wires in a seven segment display.
;;;Every segment has an ID letter between a and g. The input we have is
;;;a bunch of strings containing only those letters, but it's mixed up
;;;and we need to find the mapping between letters and segments.

;;;We don't actually need to map letters to segments. That's not neccessary.
;;;What we need to do is map strings to digits.
;;;Use the following rules to do that

;;;DIGIT RULES
;;;After finding the 4 strings with unique lengths and getting 1-segs, 4-segs,
;;;and 4-uniques, the remaining rules can be applied independently in any order
;;;8: "abcdefg"
;;;7: the only 3-char string
;;;4: the only 4-char string
;;;1: the only 2-char string
;;;1-segs: the two segments in 1
;;;4-segs: the 4 segments in 4
;;;4-uniques: the two segments that are in 4 but not 1
;;;6: the 6-char string that doesn't contain both of 1-segs
;;;9: the 6-char string that contains all of 4-segs
;;;0: the 6-char string that contains 1-segs but not 4-uniques
;;;3: the 5-char string that contains both of 1-segs
;;;5: the 5-char string that contains both of 4-uniques
;;;2: the 5-char string that does not contain 4-uniques or 1-segs

#lang racket
;Read input file into a list
(define (read-lines file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string-trim line) (read-lines file)))))

;Get just the output values from one line of input (the strings after the |)
(define (get-outputs line)
  (string-split (cadr (string-split line " | ")) " "))

;Get just the ten patterns (the strings before the |)
(define (get-patterns line)
  (string-split (car (string-split line " | ")) " "))

;Put all the chars in str into alphabetical order
;In today's input, any two strings that contain the same chars are equivalent,
;regardless of what order they're in.
;Sorting all the strings is the easiest way to deal with that.
(define (string-sort str)
  (list->string (sort (string->list str) char<?)))

;Returns true if str-a contains all the chars in str-b
(define (string-contains-all? str-a str-b)
  (andmap (λ (x) (string-contains? str-a (~a x))) (string->list str-b)))

;This function creates a hash map of strings to the digits they represent
;It's very long. Maybe some of those lambdas should be broken out into
;named functions for better readability
(define (segment-map patterns)
  (let ((1-segs (string-sort
                 (car (filter (λ (x) (= 2 (string-length x))) patterns))))
        (4-segs (string-sort
                 (car (filter (λ (x) (= 4 (string-length x))) patterns)))))
    (let ((4-uniques (list->string
                     (remove* (string->list 1-segs) (string->list 4-segs)))))
      (make-immutable-hash
       (list (cons
              (string-sort (car (filter
                    (λ (x) (and (= 6 (string-length x))
                                (string-contains-all? x 1-segs)
                                (not (string-contains-all? x 4-uniques))))
                    patterns)))
              0)
             (cons 1-segs 1)
             (cons
              (string-sort (car (filter
                    (λ (x) (and (= 5 (string-length x))
                                (not (string-contains-all? x 4-uniques))
                                (not (string-contains-all? x 1-segs))))
                    patterns)))
              2)
             (cons
              (string-sort (car (filter
                    (λ (x) (and (= 5 (string-length x))
                                (string-contains-all? x 1-segs)))
                    patterns)))
              3)
             (cons 4-segs 4)
             (cons
              (string-sort (car (filter
                    (λ (x) (and (= 5 (string-length x))
                                (string-contains-all? x 4-uniques)))
                    patterns)))
              5)
             (cons
              (string-sort (car (filter
                    (λ (x) (and (= 6 (string-length x))
                                (not (string-contains-all? x 1-segs))))
                    patterns)))
              6)
             (cons
              (string-sort (car (filter
                    (λ (x) (= 3 (string-length x)))
                    patterns)))
              7)
             (cons "abcdefg" 8)
             (cons
              (string-sort (car (filter
                    (λ (x) (and (= 6 (string-length x))
                                (string-contains-all? x 4-segs)))
                    patterns)))
              9))))))    

;Decode a single 4-digit number
(define (decode-number mappings outputs)
  (foldl (λ (x acc) (+ (* acc 10) x)) 0
         (map (λ (x) (hash-ref mappings (string-sort x))) outputs)))

;Solve part 1. The digits 1, 4, 7, and 8 contain 2, 3, 4, and 7 segments.
(define (solve1 input)
  (define (count-1478 str acc)
    (+ acc (if (member (string-length str) '(2 3 4 7)) 1 0)))
  (foldl count-1478 0 (flatten (map get-outputs input))))

;Solve part 2
(define (solve2 input)
  (foldl + 0 (map (λ (x) (decode-number (segment-map (get-patterns x))
                                        (get-outputs x)))
                  input)))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input08.txt"))
(define input (read-lines input-file))
(close-input-port input-file)

(display "Part 1: ")
(solve1 input)
(display "Part 2: ")
(solve2 input)
