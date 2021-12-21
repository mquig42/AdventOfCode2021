;;;AoC_21.rkt
;;;2021-12-21
;;;Mike Quigley

;;;Dirac Dice. That name sounds like a hint.
;;;Part 1 says it's using deterministic dice. Both parts must be deterministic,
;;;because there's always a single correct answer.

;;;This solves part 1. Part 2 requires parallel universes. An extremely large
;;;number of them. The example has a total of 786,316,482,957,123

;;;Which means that this isn't a programming challenge anymore. There's no way
;;;for a computer to simulate that many branches. This is a "wait until someone
;;;posts a math formula on Reddit, then run my input through it" challenge.
;;;That's disappointing.
;;;Maybe if I get a quantum computer, I can solve this properly.

#lang racket
;Reads a player's starting position.
(define (read-player file)
  (string->number (substring (string-trim (read-line file)) 28)))

;;Getters and setters for state list
(define (game-state die pos1 pos2 score1 score2 turn rolls)
  (list die pos1 pos2 score1 score2 turn rolls))
(define (die state)
  (first state))
(define (pos1 state)
  (second state))
(define (pos2 state)
  (third state))
(define (score1 state)
  (fourth state))
(define (score2 state)
  (fifth state))
(define (turn state)
  (sixth state))
(define (rolls state)
  (seventh state))
(define (pos player state)
  (if (= player 1) (pos1 state) (pos2 state)))
(define (score player state)
  (if (= player 1) (score1 state) (score2 state)))

(define (add-modulo a b mod)
  (+ (modulo (+ a b -1) mod) 1))

;Finds movement position based on die rolls and
(define (move die-total pos)
  (add-modulo die-total pos 10))

;Returns total value of three rolls of die with given starting value
(define (roll die)
  (let* ((roll1 die)
         (roll2 (add-modulo die 1 100))
         (roll3 (add-modulo die 2 100)))
    (+ roll1 roll2 roll3)))

(define (play-game state)
  (let* ((roll-value (roll (die state)))
         (new-pos (move roll-value (pos (turn state) state)))
         (new-score (+ (score (turn state) state) new-pos)))
    (cond ((or (>= (score1 state) 1000)
               (>= (score2 state) 1000))
           state)
          ((= 1 (turn state))
           (play-game (game-state
                       (add-modulo (die state) 3 100)
                       new-pos
                       (pos2 state)
                       new-score
                       (score2 state)
                       2
                       (+ (rolls state) 3))))
          (else (play-game (game-state
                       (add-modulo (die state) 3 100)
                       (pos1 state)
                       new-pos
                       (score1 state)
                       new-score
                       1
                       (+ (rolls state) 3)))))))

(define input-file (open-input-file "Input21.txt"))
(define player-1-start (read-player input-file))
(define player-2-start (read-player input-file))
(close-input-port input-file)

(define end-state (play-game
                   (game-state 1 player-1-start player-2-start 0 0 1 0)))

(display "Part 1: ")
(* (min (score1 end-state) (score2 end-state)) (rolls end-state))
