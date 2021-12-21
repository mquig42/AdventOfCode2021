;;;AoC_21.rkt
;;;2021-12-21
;;;Mike Quigley

;;;This might actually possible. It just reminded me too much of the space cards
;;;from 2019. Would require a cache, though. Is there a way to do that so
;;;it doesn't require my program to have global mutable state? Ie. tell Racket
;;;to cache for me. If not, I can just use a hash map.

;;;Idea for approaching this: have a function that takes a game state
;;;and returns the number of times players 1 and 2 win based on all possible
;;;dice rolls. On a winning state, it returns (1 . 0) or (0 . 1). On a
;;;non-winning state, it makes a recursive call for each die roll and sums
;;;the results. There are 27 ways to roll 3d3, with 7 possible totals.
;;;Could use the distribution to reduce the number of calls, but I doubt there's
;;;much of a speed improvement there since the extra calls would just be
;;;cache lookups anyway.

;;;Can simplify game-state. Die and rolls are no longer required.

#lang racket
(require memo)

;Reads a player's starting position.
(define (read-player file)
  (string->number (substring (string-trim (read-line file)) 28)))

;;Getters and setters for state list
(define (game-state pos1 pos2 score1 score2 turn)
  (list pos1 pos2 score1 score2 turn))
(define (pos1 state)
  (first state))
(define (pos2 state)
  (second state))
(define (score1 state)
  (third state))
(define (score2 state)
  (fourth state))
(define (turn state)
  (fifth state))
(define (pos player state)
  (if (= player 1) (pos1 state) (pos2 state)))
(define (score player state)
  (if (= player 1) (score1 state) (score2 state)))

(define (add-modulo a b mod)
  (+ (modulo (+ a b -1) mod) 1))

;Finds movement position based on dice roll
(define (move die-total pos)
  (add-modulo die-total pos 10))

;;Functions for adding pairs and lists of pairs
(define (add-pairs a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
(define (sum-pairs lst)
  (foldl add-pairs (cons 0 0) lst))

;Returns new state based on total value of dice roll
(define (next-state state roll)
  (if (= (turn state) 1)
      (game-state (move roll (pos1 state))
                  (pos2 state)
                  (+ (score1 state) (move roll (pos1 state)))
                  (score2 state)
                  2)
      (game-state (pos1 state)
                  (move roll (pos2 state))
                  (score1 state)
                  (+ (score2 state) (move roll (pos2 state)))
                  1)))

;Counts the number of universes in which each player wins,
;based on given starting state
;Need to specify hash instead of hasheq, because two identical lists
;are equal? but not eq?
(define/memoize (count-wins state) #:hash hash
  (cond ((>= (score1 state) winning-score) (cons 1 0))
        ((>= (score2 state) winning-score) (cons 0 1))
        (else
         (sum-pairs
          (map (λ (x) (count-wins (next-state state x))) rolls)))))

;;Read input
(define input-file (open-input-file "Input21.txt"))
(define player-1-start (read-player input-file))
(define player-2-start (read-player input-file))
(close-input-port input-file)

;;Constants
(define winning-score 21)
(define rolls '(3 4 4 4 5 5 5 5 5 5 6 6 6 6 6 6 6 7 7 7 7 7 7 8 8 8 9))

(display "Part 2: ")
(define wins (count-wins (game-state player-1-start player-2-start 0 0 1)))
(max (car wins) (cdr wins))