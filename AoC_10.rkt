;;;AoC_10.rkt
;;;2021-12-10
;;;Mike Quigley

;;;Input is a list of strings made of just brackets (of 4 different types)
;;;Some are corrupt (contain a closing bracket that doesn't match the most
;;;recent open bracket), and some are incomplete.

;;;The neat part is, to solve part 1, I was already making stacks of expected
;;;closing brackets, which was exactly what I needed for part 2.
;;;Also, the way I generated test-results and then filtered it 2 different ways
;;;was pretty neat too.

#lang racket
;Represent each line of input as a list of single-char strings
(define (read-lines file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (map ~a (string->list (string-trim line))) (read-lines file)))))

;Generate an intermediate result that can be fed into the scoring functions
;Corrupt strings will return the first bad char
;Incomplete strings will return a list of expected chars to complete them
(define (check-input lst)
  (define (iter openers lst)
    (cond ((null? lst) openers)
          ((string=? (car lst) "(")
           (iter (cons ")" openers) (cdr lst)))
          ((string=? (car lst) "[")
           (iter (cons "]" openers) (cdr lst)))
          ((string=? (car lst) "{")
           (iter (cons "}" openers) (cdr lst)))
          ((string=? (car lst) "<")
           (iter (cons ">" openers) (cdr lst)))
          ((string=? (car lst) (car openers))
           (iter (cdr openers) (cdr lst)))
          (else (car lst))))
  (iter '() lst))

(define (score-part-1 str)
  (cond ((string=? str ")") 3)
        ((string=? str "]") 57)
        ((string=? str "}") 1197)
        ((string=? str ">") 25137)
        (else 0)))

(define (score-part-2 lst)
  (define (iter acc lst)
    (cond ((null? lst) acc)
          ((string=? (car lst) ")")
           (iter (+ (* acc 5) 1) (cdr lst)))
          ((string=? (car lst) "]")
           (iter (+ (* acc 5) 2) (cdr lst)))
          ((string=? (car lst) "}")
           (iter (+ (* acc 5) 3) (cdr lst)))
          ((string=? (car lst) ">")
           (iter (+ (* acc 5) 4) (cdr lst)))))
  (iter 0 lst))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input10.txt"))
(define input (read-lines input-file))
(close-input-port input-file)

(define test-results (map check-input input))

(display "Part 1: ")
(foldl + 0 (map score-part-1 (filter string? test-results)))
(display "Part 2: ")
(define scores (sort (map score-part-2 (filter list? test-results)) <))
(list-ref scores (floor (/ (length scores) 2)))
