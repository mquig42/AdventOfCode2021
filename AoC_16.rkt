;;;AoC_16.rkt
;;;2021-12-16
;;;Mike Quigley

;;;Today's input looks like it might evaluate to an expression tree.
;;;That should be interesting to work with in LISP
;;;Need to decode the tree from hexadecimal first. Once I do that it'll be easy

;;;General rules: A packet is decoded into a list
;;;The first element is the version, the second is the type ID,
;;;and the last is the total bit length (so the parser knows where the next
;;;packet begins) The content is whatever's between those.
;;;For a literal packet, that will be a single number
;;;For an operator packet, that will be one or more sub-packets

#lang racket
;Converts a string of hex digits to a list of bits
(define (hex->bin hex-str)
  (define (convert-digit c)
    (cond ((eq? c #\0) '(0 0 0 0))
          ((eq? c #\1) '(0 0 0 1))
          ((eq? c #\2) '(0 0 1 0))
          ((eq? c #\3) '(0 0 1 1))
          ((eq? c #\4) '(0 1 0 0))
          ((eq? c #\5) '(0 1 0 1))
          ((eq? c #\6) '(0 1 1 0))
          ((eq? c #\7) '(0 1 1 1))
          ((eq? c #\8) '(1 0 0 0))
          ((eq? c #\9) '(1 0 0 1))
          ((eq? c #\A) '(1 0 1 0))
          ((eq? c #\B) '(1 0 1 1))
          ((eq? c #\C) '(1 1 0 0))
          ((eq? c #\D) '(1 1 0 1))
          ((eq? c #\E) '(1 1 1 0))
          ((eq? c #\F) '(1 1 1 1))))
  (flatten (map convert-digit (string->list hex-str))))

;Converts a binary list of arbitrary length to a decimal number
(define (bin->dec bin-lst)
  (foldl (λ (x acc) (+ (* acc 2) x)) 0 bin-lst))

;;Aliases for packet components
(define (pkt-version pkt)
  (car pkt))
(define (pkt-type pkt)
  (cadr pkt))
(define (pkt-content pkt)
  (caddr pkt))
(define (pkt-length pkt)
  (cadddr pkt))

;Returns a list of 4 elements:
;0: Version
;1: Type ID
;2: Value
;3: Total bit length
(define (decode-literal-packet pkt)
  (define (decode-bytes byte acc)
    (let ((value (+ (* 16 acc) (bin->dec (take (drop byte 1) 4)))))
      (if (= 0 (car byte)) value
          (decode-bytes (drop byte 5) value))))
  (define (value-length byte acc)
    (if (= 0 (car byte)) (+ acc 5)
        (value-length (drop byte 5) (+ acc 5))))
  (list (bin->dec (take pkt 3))
        (bin->dec (take (drop pkt 3) 3))
        (decode-bytes (drop pkt 6) 0)
        (value-length (drop pkt 6) 6)))

;Returns list of 4 elements
;0: Version
;1: Type ID
;2: list of contained packets
;3: total bit length
(define (decode-operator-packet pkt)
  (define (sum-packet-lengths packets)
    (foldl (λ (x acc) (+ acc (pkt-length x))) 0 packets))
  (let* ((length-type (list-ref pkt 6))
         (length-value (bin->dec (take (drop pkt 7)
                                       (if (= 0 length-type) 15 11))))
         (decoded (if (= length-type 0)
                      (decode-packets (take (drop pkt 22) length-value))
                      (decode-n-packets (drop pkt 18) length-value))))
    (list (bin->dec (take pkt 3))
          (bin->dec (take (drop pkt 3) 3))
          decoded
          (+ 7 (if (= 0 length-type) 15 11) (sum-packet-lengths decoded)))))

;Wrapper function that determines packet type and calls specific decoder
(define (decode-packet pkt)
  (cond ((null? pkt) null)
        ((and (= 1 (fourth pkt)) (= 0 (fifth pkt)) (= 0 (sixth pkt)))
         (decode-literal-packet pkt))
        (else (decode-operator-packet pkt))))

;Decodes all the packets in the given list
(define (decode-packets lst)
  (let ((decoded (decode-packet lst)))
    (if (null? decoded) null
        (cons decoded (decode-packets (drop lst (pkt-length decoded)))))))

;Decodes the first n packets found in lst
(define (decode-n-packets lst n)
  (if (= n 0) null
      (let ((decoded (decode-packet lst)))
        (cons decoded
              (decode-n-packets (drop lst (pkt-length decoded)) (- n 1))))))

;Returns the sum of all version numbers contained in pkt, a decoded packet
(define (sum-versions pkt)
  (if (= 4 (second pkt)) (pkt-version pkt)
      (+ (car pkt)
         (foldl (λ (x acc) (+ (sum-versions x) acc)) 0 (pkt-content pkt)))))

;Evaluates the value of a decoded packet
;It might be fun to transform packets into something that the LISP parser
;can evaluate, but for now I did it the easy way
(define (pkt-eval pkt)
  (cond ((= (pkt-type pkt) 0)
         (foldl (λ (x acc) (+ acc (pkt-eval x))) 0 (pkt-content pkt)))
        ((= (pkt-type pkt) 1)
         (foldl (λ (x acc) (* acc (pkt-eval x))) 1 (pkt-content pkt)))
        ((= (pkt-type pkt) 2)
         (foldl (λ (x acc) (min acc (pkt-eval x))) +inf.0 (pkt-content pkt)))
        ((= (pkt-type pkt) 3)
         (foldl (λ (x acc) (max acc (pkt-eval x))) -inf.0 (pkt-content pkt)))
        ((= (pkt-type pkt) 4) (pkt-content pkt))
        ((= (pkt-type pkt) 5)
         (if (> (pkt-eval (first (pkt-content pkt)))
                (pkt-eval (second (pkt-content pkt)))) 1 0))
        ((= (pkt-type pkt) 6)
         (if (< (pkt-eval (first (pkt-content pkt)))
                (pkt-eval (second (pkt-content pkt)))) 1 0))
        ((= (pkt-type pkt) 7)
         (if (= (pkt-eval (first (pkt-content pkt)))
                (pkt-eval (second (pkt-content pkt)))) 1 0))))

;;Read input
(define input-file (open-input-file "Input16.txt"))
(define input (hex->bin (string-trim (read-line input-file))))
(close-input-port input-file)

;;Display output
(define parsed-input (decode-packet input))
(display "Part 1: ")
(sum-versions parsed-input)
(display "Part 2: ")
(inexact->exact (pkt-eval parsed-input))
