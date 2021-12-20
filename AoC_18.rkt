;;;AoC_18.rkt
;;;2021-12-18
;;;Mike Quigley

;;;This description started out like LISP, with values being stored in
;;;a series of nested pairs. Then it went to "If any pair is nested inside four
;;;pairs, the leftmost such pair explodes." What?

;;;Progress update: Everything works, most of the time.
;;;There is a problem with get-neighbours caused by flattening the tree
;;;Consider the following:
;;;((((5 11) (13 0)) ((15 14) (0 (14 0))))...
;;;The (14 0) pair will explode, but if you flatten this, you will see
;;;14 and 0 before that (even though they're not in the same pair.
;;;How about converting to a string and using regular expressions

;;;That works until the exploder isn't unique. Consider this:
;;;((((6 7) (0 7)) ((7 (6 7)) (0 21))) ((2 (11 10)) ((0 8) (8 0))))
;;;          This pair ^^^^^ explodes, but there is another (6 7) before it
;;;and the explode function was exploding that instead of the correct one

;;;Maybe get-exploder should return a string with the exploding pair
;;;flagged in some way that would be easy to pick up on and reassemble
;;;like "((((6 7) (0 7)) ((7 (E6 7X)) (0 21))) ((2 (11 10)) ((0 8) (8 0))))"

;;;Or add a million to both numbers to make it unique

#lang racket
(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (decode-line line) (read-input file)))))

;Converts a string from the input file into an s-expression
(define (decode-line str)
  (read (open-input-string
         (string-replace
          (string-replace
           (string-replace
            (string-trim str)
            "," " ")
           "[" "(")
          "]" ")"))))

;Return the left or right member of a pair
(define (left expr)
  (car expr))
(define (right expr)
  (cadr expr))

;Adds two snailfish expressions
(define (add a b)
  (list a b))

;Find the maximum depth of expr, to see if anything should explode
(define (max-depth expr depth)
  (if (number? expr) depth
      (max (max-depth (left expr) (+ depth 1))
           (max-depth (right expr) (+ depth 1)))))

;Find the maximum value of expr, to see if anything should split
(define (max-value expr)
  (if (number? expr) expr
      (max (max-value (left expr))
           (max-value (right expr)))))

;Splits the leftmost value >= 10
;Calls max-value, so probably inefficient
(define (split expr)
  (cond ((and (number? expr) (> expr 9)) ;Split this number
         (list (floor (/ expr 2)) (ceiling (/ expr 2))))
        ((> (max-value (left expr)) 9) ;The value to split is to the left
         (list (split (left expr)) (right expr)))
        ((> (max-value (right expr)) 9) ;The value to split is to the right
         (list (left expr) (split (right expr))))
        (else expr))) ;If no children have values over 10, just return

;Explode
(define (explode expr)
  (define (replace-left str from to)
    (if from
        (string-reverse
         (string-replace
          (string-reverse str)
          (string-reverse (~a from))
          (string-reverse (~a (+ from to)))
          #:all? false))
        str))
  (define (replace-right str from to)
    (if from
        (string-replace str
                        (~a from)
                        (~a (+ from to))
                        #:all? false)
        str))
  (let* ((exploder (get-exploder expr))
         (marked-exploder (list (+ (left exploder) 1000000)
                                (+ (right exploder) 1000000)))
         (marked-expr (~a (mark-exploder expr)))
         (neighbours (get-neighbours marked-expr marked-exploder))
         (expr-str (string-split marked-expr (~a marked-exploder))))
    (decode-line
     (string-append
      (replace-left (car expr-str)
                    (left neighbours)
                    (left exploder))
      "0"
      (replace-right (cadr expr-str)
                     (right neighbours)
                     (right exploder))))))

;Returns the pair which will explode
(define (get-exploder expr)
  (define (helper expr depth)
    (cond ((= depth 4) expr)
          ((>= (max-depth (left expr) depth) 4)
           (helper (left expr) (+ depth 1)))
          ((>= (max-depth (right expr) depth) 4)
           (helper (right expr) (+ depth 1)))
          (else null)))
  (helper expr 0))

;Adds 1000000 to each number in the exploding pair
;This will ensure that it is unique
(define (mark-exploder expr)
  (define (helper expr depth)
    (cond ((>= depth 4) (list (+ (car expr) 1000000)
                             (+ (cadr expr) 1000000)))
          ((>= (max-depth (left expr) depth) 4)
           (list (helper (left expr) (+ 1 depth)) (right expr)))
          ((>= (max-depth (right expr) depth) 4)
           (list (left expr) (helper (right expr) (+ 1 depth))))
          (else expr)))
  (helper expr 0))

;New implementation of get-neighbours using regular expressions
;Takes expr as a string
(define (get-neighbours expr-str exploder)
  (let* ((quoted-exploder (regexp-quote (~a exploder)))
         (left-neighbour
          (regexp-match (pregexp (string-append "\\d+(?=[\\(\\) ]+"
                                                quoted-exploder
                                                ")"))
                        expr-str))
         (right-neighbour
          (regexp-match (pregexp (string-append "(?<="
                                                quoted-exploder
                                                ")[\\(\\) ]+\\d+"))
                        expr-str)))
    (list (if left-neighbour (string->number
                              (car left-neighbour))
              left-neighbour)
          (if right-neighbour (string->number
                               (regexp-replace #px"\\D*"
                                               (car right-neighbour)
                                               ""))
              right-neighbour))))

;Can't believe this isn't a standard function
(define (string-reverse str)
  (list->string (reverse (string->list str))))
    

;Applies explode and split until expr is fully reduced
(define (reduce expr)
  (cond ((> (max-depth expr 0) 4) (reduce (explode expr)))
        ((> (max-value expr) 9) (reduce (split expr)))
        (else expr)))

;Calculates the magnitude of expr
(define (mag expr)
  (if (number? expr) expr
      (+ (* 3 (mag (left expr)))
         (* 2 (mag (right expr))))))

;Finds the sum of all numbers in input
(define (sum-input)
  (foldl (λ (x acc) (reduce (add acc x))) (car input) (cdr input)))

;Finds the maximum magnitude of adding any two numbers from input
(define (solve2)
  (define (max-mag a b)
    (max (mag (reduce (add a b)))
         (mag (reduce (add b a)))))
  (argmax identity (map (λ (x) (max-mag (car x) (cadr x)))
                        (combinations input 2))))

;;Read input
(define input-file (open-input-file "Input18.txt"))
(define input (read-input input-file))
(close-input-port input-file)

;;Display output
(display "Part 1: ")
(mag (sum-input))
(display "Part 2: ")
(solve2)