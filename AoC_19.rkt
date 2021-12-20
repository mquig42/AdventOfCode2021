;;;AoC_19.rkt
;;;2021-12-19
;;;Mike Quigley

;;;So, this is like the sea monster problem from last year, only even *more*
;;;complicated. Instead of every tile having unique and easily matchable edges,
;;;this year they overlap, so I need to match based on their actual content.

;;;Also, it's in 3D. This means the tiles can be rotated to 24 different
;;;orientations.

;;;I already spent much of today solving yesterday's puzzle, so finishing this
;;;may have to wait. Let's try to at least read the input.

;;;Done. NOTE: file must end with two newlines, not just one

#lang racket
;Read a single scan from the input file
(define (read-scan file)
  (define (read-coords)
    (let ((str (string-trim (read-line file))))
      (cond ((string=? str "") null)
            (else
             (cons (map string->number (string-split str ","))
                   (read-coords))))))
  (if (eof-object? (read-line file)) null
      (read-coords)))

;Read all the scans from the input file
(define (read-scans file)
  (let ((scan (read-scan file)))
    (if (null? scan) null
        (cons scan (read-scans file)))))

(define input-file (open-input-file "Test19.txt"))
(define input (read-scans input-file))
(close-input-port input-file)
