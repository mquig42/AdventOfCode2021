;;;AoC_10.rkt
;;;2021-12-10
;;;Mike Quigley

;;;

#lang racket
(define (read-lines file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (map ~a (string->list (string-trim line))) (read-lines file)))))

(define (first-bad-char lst)
  (define (iter openers lst)
    (cond ((null? lst) " ")
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

(define (score str)
  (cond ((string=? str ")") 3)
        ((string=? str "]") 57)
        ((string=? str "}") 1197)
        ((string=? str ">") 25137)
        (else 0)))

;;Entry Point ==================================================================
(define input-file (open-input-file "Input10.txt"))
(define input (read-lines input-file))
(close-input-port input-file)

(display "Part 1: ")
(foldl + 0 (map score (map first-bad-char input)))
