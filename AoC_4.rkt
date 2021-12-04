;;;AoC_4.txt
;;;2021-12-04
;;;Mike Quigley

;;;Today we're playing bingo with a squid. Might be at a disadvantage here,
;;;he can hold like ten markers at once.
;;;Input is a sequence of numbers, followed by a sequence of bingo cards
;;;For part 1, find the winning card and its score
;;;For part 2, find the losing card and its score. 

;;;For the first time, I'll need to read the input in a more complicated way
;;;than just putting everything into one big list.
;;;There are three challenges here
;;;1. Read the draw sequence and all cards, and represent them in memory
;;;2. Figure out when in the draw sequence each card wins
;;;3. Calculate the final score of any given card

;First, use full-on Racket instead of SICP. Need some extra io functions.
#lang racket

;To read one line of a Bingo card, simply read 5 items into a list
(define (read-card-line file)
  (list (read file)
        (read file)
        (read file)
        (read file)
        (read file)))

;To read a whole card, call read-card-line five times
(define (read-card file)
  (list (read-card-line file)
        (read-card-line file)
        (read-card-line file)
        (read-card-line file)
        (read-card-line file)))

;Reads a fixed number of cards. That way we don't need to check for EOF
(define (read-n-cards n file)
  (if (= 0 n) null
      (cons (read-card file) (read-n-cards (- n 1) file))))

;Generate a sequence of integers from start to end
(define (make-seq start end)
  (if (> start end) null
      (cons start (make-seq (+ start 1) end))))

;Simple linear search. Returns position of n
(define (position-of n lis)
  (define (iter acc n lis)
    (if (= n (car lis)) acc
        (iter (+ acc 1) n (cdr lis))))
  (iter 0 n lis))

;Returns nth item of a list
(define (item-at n lis)
  (if (= n 0) (car lis)
      (item-at (- n 1) (cdr lis))))

;Might change draw-sequence from a list to a dictionary later.
;Making wrappers now to make that easier
(define (draw-pos n)
  (position-of n draw-sequence))

(define (nth-draw n)
  (item-at n draw-sequence))

;Flexible function that can find the maximum or minimum value of a list
(define (list-val-helper initial comparison lis)
  (cond ((null? lis) initial)
        ((comparison (car lis) initial)
         (list-val-helper (car lis) comparison (cdr lis)))
        (else (list-val-helper initial comparison (cdr lis)))))

;Returns maximum value of a list
(define (max-val lis)
  (list-val-helper 0 > lis))

;Returns minimum value of a list
(define (min-val lis)
  (list-val-helper 99999 < lis))

;Helper function for finding position of lowest or highest item in a list
(define (list-pos-helper pos pos-r initial comparison lis)
  (cond ((null? lis) pos-r)
        ((comparison (car lis) initial)
         (list-pos-helper (+ pos 1) pos (car lis) comparison (cdr lis)))
        (else (list-pos-helper (+ pos 1) pos-r initial comparison (cdr lis)))))

;Returns the position of the lowest item in a list
(define (pos-of-lowest lis)
  (list-pos-helper 0 0 99999 < lis))

;Returns the position of the highest item in a list
(define (pos-of-highest lis)
  (list-pos-helper 0 0 0 > lis))

;Returns nth row of card
(define (row n card)
  (item-at n card))

;Returns nth column of card
(define (col n card)
  (map (λ (x) (item-at n (row x card))) list5))

;Returns the draw-position of the last number to be drawn in this card
(define (time-to-win card)
  ;Calculates time to win for a single row or column
  (define (time-to-win-row lis)
    (max-val (map draw-pos lis)))
  
  ;Returns all rows and columns in a card
  (define (enumerate card)
    (append card (map (λ (x) (col x card)) list5)))
  
  (min-val (map time-to-win-row (enumerate card))))

;Returns the index of the winning card
(define (pos-winning-card)
  (pos-of-lowest (map time-to-win cards)))

;Returns the index of the losing card (last card to win)
(define (pos-losing-card)
  (pos-of-highest (map time-to-win cards)))

;Finds the final score of a card at the time that it wins,
;assuming no other cards win first
(define (score card)
  (define (sum-unmarked acc num-draws card)
    (cond ((null? card) acc)
          ((< num-draws (draw-pos (car card)))
           (sum-unmarked (+ acc (car card)) num-draws (cdr card)))
          (else (sum-unmarked acc num-draws (cdr card)))))
  (let ((num-draws (time-to-win card)))
    (* (sum-unmarked 0 num-draws (flatten card)) (nth-draw num-draws))))

;;Entry Point ==================================================================

(define filename "Input4.txt")
(define number-of-cards 100)
(define list5 '(0 1 2 3 4))
(define input-file (open-input-file filename))

;First line is the draw sequence. Read it into a list.
(define draw-sequence
  (map string->number (string-split (string-trim (read-line input-file)) ",")))

;Now read all cards
(define cards (read-n-cards number-of-cards input-file))

(close-input-port input-file)

;Find and display output
(display "Part 1:")
(newline)
(display "Winning Card: ")
(pos-winning-card)
(display "       Draws: ")
(time-to-win (item-at (pos-winning-card) cards))
(display "       Score: ")
(score (item-at (pos-winning-card) cards))

(newline)
(display "Part 2:")
(newline)
(display " Losing Card: ")
(pos-losing-card)
(display "       Draws: ")
(time-to-win (item-at (pos-losing-card) cards))
(display "       Score: ")
(score (item-at (pos-losing-card) cards))
