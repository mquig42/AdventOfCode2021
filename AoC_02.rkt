;;;AoC_02.rkt
;;;2021-12-02
;;;Mike Quigley

;;;Today we're interpreting commands to drive a submarine in two different ways
;;;Fairly straightforward once I read the input. The docs on file I/O are sparse
;;;Both of my solutions use tail recursion, for whatever trivial bit of
;;;memory efficiency that's worth. The input is only 9kb.

#lang sicp
;Generates a list from a text file.
;Any whitespace in the file will act as a separator
(define (read-list file)
  (let ((line (read file)))
    (if (eof-object? line)
        nil
        (cons line (read-list file)))))

(define (add-pair a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

;Let's store these commands as pairs. The car is horizontal pos,
;and the cdr is vertical
;NOTE: the text in the file is being read as symbols, not strings.
;This may not be ideal. Not sure what the implications are
(define (decode-direction direction distance)
  (cond ((eq? direction 'forward) (cons distance 0))
        ((eq? direction 'down) (cons 0 distance))
        ((eq? direction 'up) (cons 0 (* -1 distance)))
        (else (cons 0 0))))

;Follow directions using the rules from part 1
(define (follow-directions-one initial directions)
  (cond ((null? directions) initial)
        (else (follow-directions-one
               (add-pair initial
                         (decode-direction (car directions) (cadr directions)))
               (cddr directions)))))

;Follow directions using rules from part 2
;I didn't use a separate decoder here, so this is self-contained
(define (follow-directions-two initial aim directions)
  (cond ((null? directions) initial)
        ((eq? (car directions) 'forward)
         (follow-directions-two
          (add-pair initial (cons (* aim (cadr directions)) (cadr directions)))
          aim
          (cddr directions)))
        (else
         (follow-directions-two
          initial
          (+ aim (if (eq? (car directions) 'up)
                     (* -1 (cadr directions))
                     (cadr directions)))
          (cddr directions)))))

(define input-file (open-input-file "Input2.txt"))
(define input (read-list input-file))
(close-input-port input-file)

(display "Part 1: ")
(define end-pos-one (follow-directions-one (cons 0 0) input))
(* (car end-pos-one) (cdr end-pos-one))
(display "Part 2: ")
(define end-pos-two (follow-directions-two (cons 0 0) 0 input))
(* (car end-pos-two) (cdr end-pos-two))
