;;;AoC_24.rkt
;;;2021-12-24
;;;Mike Quigley

;;;Tried to brute force this. Didn't work.
;;;At least I can use it to test numbers I come up with by hand

#lang racket
;Read all lines from file
(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string-trim line) (read-input file)))))

;Convert string into parsed instruction
(define (parse-instructions input)
  (map (λ (line)
         (map (λ (token) (read (open-input-string token)))
                (string-split line)))
       input))

;Turn number into list of digits
;This is based on string manipulation. Also try math, see if it's faster.
(define (number->list n)
  (map (λ (x) (string->number (~a x))) (string->list (~a n))))

;Getters and setters for registers
(define (r-set registers name value)
  (list-set registers (hash-ref r-addr name) value))
(define (r-get registers name)
  (if (number? name) name
      (list-ref registers (hash-ref r-addr name))))

;VM that runs puzzle input as a program
(define (run registers program data)
  (if (null? program) (cadddr registers)
      (let ((line (car program)))
        (cond ((eq? 'inp (car line))
               (run (r-set registers (cadr line) (car data))
                    (cdr program)
                    (cdr data)))
              ((eq? 'add (car line))
               (run (r-set registers (cadr line)
                           (+ (r-get registers (cadr line))
                              (r-get registers (caddr line))))
                    (cdr program)
                    data))
              ((eq? 'mul (car line))
               (run (r-set registers (cadr line)
                           (* (r-get registers (cadr line))
                              (r-get registers (caddr line))))
                    (cdr program)
                    data))
              ((eq? 'div (car line))
               (run (r-set registers (cadr line)
                           (truncate (/ (r-get registers (cadr line))
                                        (r-get registers (caddr line)))))
                    (cdr program)
                    data))
              ((eq? 'mod (car line))
               (run (r-set registers (cadr line)
                           (modulo (r-get registers (cadr line))
                                   (r-get registers (caddr line))))
                    (cdr program)
                    data))
              ((eq? 'eql (car line))
               (run (r-set registers (cadr line)
                           (if (= (r-get registers (cadr line))
                                  (r-get registers (caddr line))) 1 0))
                           (cdr program)
                           data))))))

;;Read input
(define input-file (open-input-file "Input24.txt"))
(define program (parse-instructions (read-input input-file)))
(close-input-port input-file)

(define r-addr (hash 'w 0 'x 1 'y 2 'z 3))

(define modelnum 95299897999897)
(run '(0 0 0 0) program (number->list modelnum))
