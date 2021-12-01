;;;AoC_1.rkt
;;;2021-12-01
;;;Mike Quigley

;;;Analyze the data from a submarine's depth sounder
;;;The input is a list of depth measurements. For part 1, simply
;;;find the number of measurements that are greater than the previous one
;;;For part 2, need to use a sliding window of 3 measurements
;;;If I make a list of all the sums of 3 measurements, I can just use
;;;the function I made for part 1.

#lang sicp
;Generates a list from a text file.
;Any whitespace in the file will act as a separator
(define (read-list filename)
  (define (close-file-nil file)
    (close-input-port file)
    nil)
  (define (read-list-iter file)
    (let ((line (read file)))
      (if (eof-object? line)
          (close-file-nil file)
          (cons line (read-list-iter file)))))
  (let ((file (open-input-file filename)))
    (read-list-iter file)))

;Count the number of items in a list which are larger than the pervious item
(define (count-increases initial depths)
  (cond ((null? depths) 0)
        ((< initial (car depths))
         (+ 1 (count-increases (car depths) (cdr depths))))
        (else (count-increases (car depths) (cdr depths)))))

;Make a list based on the 3 entry sliding window for part 2
;Each item will be the sum of three items from the input list
(define (window depths)
  (cond ((null? depths) nil)
        ((null? (cdr depths)) nil)
        ((null? (cddr depths)) nil)
        (else (cons
               (+ (car depths) (cadr depths) (caddr depths))
               (window (cdr depths))))))

(define input (read-list "Input1.txt"))

(display "Part 1: ")
(count-increases 9999 input)

(display "Part 2: ")
(count-increases 9999 (window input))
