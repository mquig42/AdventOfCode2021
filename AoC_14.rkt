;;;AoC_14.rkt
;;;2021-12-14
;;;Mike Quigley

;;;Currently can read data and do the insertions. Each insertion runs in linear
;;;time, but the length of the list will increase significantly each round.
;;;After some experimentation, this approach really can't go much past 10.

;;;Part 2 asks for 40 rounds of insertion. I estimate this could take up to
;;;20TB of RAM. There must be a way to do this without actually generating
;;;the polymer string.

;;;Try finding the frequencies of each letter after each of 10 rounds
;;;and doing regression analysis. That might not be precise enough, but
;;;I don't have any better ideas.

#lang racket
(define (read-insertions file)
  (let ((line (read-line file)))
    (cond ((eof-object? line) null)
          ((string=? line "\r") (read-insertions file))
          (else (let ((sp (string-split (string-trim line) " -> ")))
                  (cons (cons (car sp) (string-ref (cadr sp) 0))
                        (read-insertions file)))))))

;;Polymer insertion functions
(define (insert tpl)
  (cond ((null? (cdr tpl)) tpl)
        ((dict-has-key? insertions (string (car tpl) (cadr tpl)))
         (cons (car tpl)
               (cons (hash-ref insertions (string (car tpl) (cadr tpl)))
                     (insert (cdr tpl)))))
        (else (cons (car tpl) (insert (cdr tpl))))))

(define (insert-n tpl n)
  (foldl (λ (x acc) (insert acc)) tpl (range n)))
        
;;Read input
(define input-file (open-input-file "Input14.txt"))
(define template (string->list (string-trim (read-line input-file))))
(define insertions (make-immutable-hash (read-insertions input-file)))
(close-input-port input-file)

(define alphabet (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define p1-results (insert-n template 10))
(define frequencies
  (filter (λ (z) (> z 0))
          (map (λ (x) (count (λ (y) (eq? x y)) p1-results)) alphabet)))

;;Run and display output
(display "Part 1: ")
(- (argmax identity frequencies) (argmin identity frequencies))
