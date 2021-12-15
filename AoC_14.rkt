;;;AoC_14.rkt
;;;2021-12-14
;;;Mike Quigley

;;;Had to completely rewrite this for part 2
;;;Uses a similar solution to the lanternfish problem. Instead of doing inserts
;;;to generate the polymer as a list, use counters.
;;;Need to keep a track of every pair and every individual char, since
;;;pair counts are needed for the insertions but char count is what we want.

#lang racket
(require racket/hash)
;Each insertion puts a letter in the middle of a pair.
;This adds one new letter to the polymer chain, and creates two new pairs
;Create a structure containing both pairs and the letter, as we need to track
;the counts of all of those. All the fields in the input are fixed-width,
;so just use string-ref and substring to read it.
(define (read-insertions file)
  (let ((line (read-line file)))
    (cond ((eof-object? line) null)
          ((string=? line "\r") (read-insertions file))
          (else (cons (list (substring line 0 2)
                            (string (string-ref line 0) (string-ref line 6))
                            (string (string-ref line 6) (string-ref line 1))
                            (string-ref line 6))
                      (read-insertions file))))))

;Makes a hash table containing counts of each distinct char in str
(define (count-chars str)
  (foldl (λ (x acc) (hash-set acc x (+ (hash-ref acc x 0) 1)))
         (hash)
         (string->list str)))

;Makes a hash table containing counts of each distinct pair of letters in str
(define (count-pairs str)
  (foldl (λ (x acc) (hash-set acc
                              (substring str x (+ x 2))
                              (+ (hash-ref acc (substring str x (+ x 2)) 0) 1)))
         (hash)
         (range (- (string-length str) 1))))

;Do one round of insertions
(define (insert counts)
  (foldl (λ (x acc)
           (if (dict-has-key? counts (car x))
               ;Increment counter for first pair created by this insertion
               (let ((first-pair
                      (hash-set acc (cadr x)
                                (+ (hash-ref counts (car x))
                                   (hash-ref acc (cadr x) 0)))))
                 ;Increment counter for second pair created by this insertion
                 (let ((second-pair
                        (hash-set first-pair (caddr x)
                                  (+ (hash-ref counts (car x))
                                     (hash-ref acc (caddr x) 0)))))
                   ;Increment counter for the char that was inserted
                   (let ((middle-char
                          (hash-set second-pair (cadddr x)
                                    (+ (hash-ref counts (car x))
                                       (hash-ref acc (cadddr x) 0)))))
                     ;Inserting into the middle of a pair destroys that pair
                     ;More instances of it may have been created by insertions
                     ;Decrement count by its initial value
                     (hash-set middle-char (car x)
                               (- (hash-ref middle-char (car x))
                                  (hash-ref counts (car x)))))))
               acc))
         counts
         insertions))

;Runs n rounds of insertions
(define (insert-n n)
  (foldl (λ (x acc) (insert acc)) initial (range n)))

;Finds the most and least common elements and returns the puzzle solution
(define (solve n)
  (let ((result (insert-n n)))
    (let ((elements (filter char? (hash-keys result))))
      (let ((common-element (argmax (λ (x) (hash-ref result x)) elements))
            (rare-element (argmin (λ (x) (hash-ref result x)) elements)))
        (- (hash-ref result common-element) (hash-ref result rare-element))))))

;;Read input
(define input-file (open-input-file "Input14.txt"))
(define template (string-trim (read-line input-file)))
(define insertions (read-insertions input-file))
(close-input-port input-file)

;;Compute initial char and pair counts from template
(define initial (hash-union (count-chars template) (count-pairs template)))

;;Solve and display output
(display "Part 1: ")
(solve 10)
(display "Part 2: ")
(solve 40)
