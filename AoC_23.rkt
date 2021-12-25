;;;AoC_23.rkt
;;;2021-12-23
;;;Mike Quigley

;;;Today we're helping a bunch of amphipods get back to their rooms
;;;It may be possible to enumerate every path, since valid moves are so
;;;restricted. Each amphipod moves at most twice. Once into the hall, then
;;;once into its room. It may also move directly from one room to another.
;;;There are 4 rooms and 7 hallway locations.
;;;Need to come up with a good data structure to represent state,
;;;and a function to enumerate all valid moves.

;;;Number of moves is limited. Each amphipod can move from the room it's in
;;;to the hall, or from the hall to its own room. Once it's in its own room
;;;it stays there, so each amphipod can make two moves.
;;;Amphipods in the hall block each other

;;;#############
;;;#12.3.4.5.67#
;;;###8#9#A#B###
;;;  #C#D#E#F#
;;;  #G#H#I#J#
;;;  #K#L#M#N#
;;;  #########

;;;Solved part 1 manually, so this program is for part 2
;;;It takes almost a minute to run, but it gets the right answer in the end
;;;Maybe Dijkstra's algorithm would be faster than memoized recursion,
;;;but this program is already too long.

;;;Edit: Figured out a simple way to make it also solve part 1.
;;;Just feed it a starting state with the bottom half of each room
;;;already solved

#lang racket
(require memo)

;;Define functions
;Returns new state after move
(define (move state from to)
  (list-set
   (list-set state from #\.)
   to (list-ref state from)))

;Validate a potential move
(define (is-valid? state from to)
  (cond ((eq? (list-ref state from) #\.) false)
        ((not (eq? (list-ref state to) #\.)) false)
        ((and (member from hall) (member to hall)) false)
        ((and (not (member from hall)) (not (member to hall))) false)
        ((and (eq? (list-ref state from) #\A)
              (member from hall)
              (not (member to room-a))) false)
        ((and (eq? (list-ref state from) #\B)
              (member from hall)
              (not (member to room-b))) false)
        ((and (eq? (list-ref state from) #\C)
              (member from hall)
              (not (member to room-c))) false)
        ((and (eq? (list-ref state from) #\D)
              (member from hall)
              (not (member to room-d))) false)
        ((and (eq? (list-ref state from) #\A)
              (not (member to hall))
              (not (is-valid-room? state #\A room-a to))) false)
        ((and (eq? (list-ref state from) #\B)
              (not (member to hall))
              (not (is-valid-room? state #\B room-b to))) false)
        ((and (eq? (list-ref state from) #\C)
              (not (member to hall))
              (not (is-valid-room? state #\C room-c to))) false)
        ((and (eq? (list-ref state from) #\D)
              (not (member to hall))
              (not (is-valid-room? state #\D room-d to))) false)
        ((and (eq? (list-ref state from) #\A)
              (member from room-a)
              (is-valid-room? state #\A room-a from)) false)
        ((and (eq? (list-ref state from) #\B)
              (member from room-b)
              (is-valid-room? state #\B room-b from)) false)
        ((and (eq? (list-ref state from) #\C)
              (member from room-c)
              (is-valid-room? state #\C room-c from)) false)
        ((and (eq? (list-ref state from) #\D)
              (member from room-d)
              (is-valid-room? state #\D room-d from)) false)
        ((blocked? state from to) false)
        (else true)))

;Can this room be moved into? Does not check if destination is empty.
(define (is-valid-room? state amphipod room to)
  (cond ((and (= to (car room))
              (eq? amphipod (list-ref state (cadr room)))
              (eq? amphipod (list-ref state (caddr room)))
              (eq? amphipod (list-ref state (cadddr room)))) true)
        ((and (= to (cadr room))
              (eq? amphipod (list-ref state (caddr room)))
              (eq? amphipod (list-ref state (cadddr room)))) true)
        ((and (= to (caddr room))
              (eq? amphipod (list-ref state (cadddr room)))) true)
        ((= to (cadddr room)) true)
        (else false)))

;Is the path blocked? This may be extremely tedious to write
;unless I find a shortcut
(define (blocked? state from to)
  (define (iter path)
    (cond ((null? path) false)
          ((not (eq? #\. (list-ref state (car path)))) true)
          (else (iter (cdr path)))))
  (iter (enumerate-spaces from to)))

;I found a shortcut. The list of paths includes every point on every valid route
;Just need to find the list that includes both from and to, and can enumerate
;all intermediate points and see if they're clear.
(define (enumerate-spaces from to)
  (let* ((paths '((0 1 7 11 15 19)
                  (19 15 11 7 2 3 4 5 6)
                  (0 1 2 8 12 16 20)
                  (20 16 12 8 3 4 5 6)
                  (0 1 2 3 9 13 17 21)
                  (21 17 13 9 4 5 6)
                  (0 1 2 3 4 10 14 18 22)
                  (22 18 14 10 5 6)))
         (path (car (filter (λ (x) (and (member from x) (member to x))) paths)))
         (pos-from (get-pos path from))
         (pos-to (get-pos path to)))
    (take (drop path (min pos-from pos-to))
          (- (abs (- pos-from pos-to)) 1))))

;Get 1-indexed position of element in list
(define (get-pos lst sf)
  (define (iter lst n)
    (cond ((null? lst) false)
          ((= (car lst) sf) n)
          (else (iter (cdr lst) (+ n 1)))))
  (iter lst 1))

;Calculates movement cost
(define (movement-cost amphipod from to)
  (* (hash-ref movement-costs amphipod)
     (list-ref (list-ref distances from) to)))

;Given a state, returns a list of all valid moves in (from . to) pairs
(define (enumerate-moves state)
  (define (enumerate-moves-from from)
    (foldl (λ (to acc) (if (is-valid? state from to) (cons (cons from to) acc)
                           acc))
           null
           (range 23)))
  (foldl (λ (from acc) (append (enumerate-moves-from from) acc))
         null
         (range 23)))

;Memoized recursive function that finds the cheapest solution from given state
(define/memoize (cost-to-solve state acc) #:hash hash
  (cond ((equal? state goal) acc)
        (else
         (let ((costs (map (λ (x) (cost-to-solve (move state (car x) (cdr x))
                                                 (+ (movement-cost
                                                     (list-ref state (car x))
                                                     (car x)
                                                     (cdr x))
                                                    acc)))
                           (enumerate-moves state))))
           (if (null? costs) +inf.0
               (argmin identity costs))))))

;Prints a map showing the positions of all amphipods. Not actually used.
(define (print-state state)
  (display "#############")
  (newline)
  
  (display #\#)
  (display (list-ref state 0))
  (display (list-ref state 1))
  (display #\.)
  (display (list-ref state 2))
  (display #\.)
  (display (list-ref state 3))
  (display #\.)
  (display (list-ref state 4))
  (display #\.)
  (display (list-ref state 5))
  (display (list-ref state 6))
  (display #\#)
  (newline)

  (display "###")
  (display (list-ref state 7))
  (display #\#)
  (display (list-ref state 8))
  (display #\#)
  (display (list-ref state 9))
  (display #\#)
  (display (list-ref state 10))
  (display "###")
  (newline)

  (display "  #")
  (display (list-ref state 11))
  (display #\#)
  (display (list-ref state 12))
  (display #\#)
  (display (list-ref state 13))
  (display #\#)
  (display (list-ref state 14))
  (display #\#)
  (newline)

  (display "  #")
  (display (list-ref state 15))
  (display #\#)
  (display (list-ref state 16))
  (display #\#)
  (display (list-ref state 17))
  (display #\#)
  (display (list-ref state 18))
  (display #\#)
  (newline)

  (display "  #")
  (display (list-ref state 19))
  (display #\#)
  (display (list-ref state 20))
  (display #\#)
  (display (list-ref state 21))
  (display #\#)
  (display (list-ref state 22))
  (display #\#)
  (newline)
  (display "  #########")
  (newline))

;;Define constants
;Distance matrix.
(define distances '((0 1 3 5 7 9 10 3 5 7 9 4 6 8 10 5 7 9 11 6 8 10 12);1
                    (1 0 2 4 6 8 9 2 4 6 8 3 5 7 9 4 6 8 10 5 7 9 11);2
                    (3 2 0 2 4 6 7 2 2 4 6 3 3 5 7 4 4 6 8 5 5 7 9);3
                    (5 4 2 0 2 4 5 4 2 2 4 5 3 3 5 6 4 4 6 7 5 5 7);4
                    (7 6 4 2 0 2 3 6 4 2 2 7 5 3 3 8 6 4 4 9 7 5 5);5
                    (9 8 6 4 2 0 1 8 6 4 2 9 7 5 3 10 8 6 4 11 9 7 5);6
                    (10 9 7 5 3 1 0 9 7 5 3 10 8 6 4 11 9 7 5 12 10 8 6);7
                    (3 2 2 4 6 8 9 0 4 6 8 1 5 7 9 2 6 8 10 3 7 9 11);8
                    (5 4 2 2 4 6 7 4 0 4 6 5 1 5 7 6 2 6 8 7 3 7 9);9
                    (7 6 4 2 2 4 5 6 4 0 4 7 5 1 5 8 6 2 6 9 7 3 7);A
                    (9 8 6 4 2 2 3 8 6 4 0 9 7 5 1 10 8 6 2 11 9 7 3);B
                    (4 3 3 5 7 9 10 1 5 7 9 0 6 8 10 1 7 9 11 2 8 10 12);C
                    (6 5 3 3 5 7 8 5 1 5 7 6 0 6 8 7 1 7 9 8 2 8 10);D
                    (8 7 5 3 3 5 6 7 5 1 5 8 6 0 6 9 7 1 7 10 8 2 8);E
                    (10 9 7 5 3 3 4 9 7 5 1 10 8 6 0 11 9 7 1 12 10 8 2);F
                    (5 4 4 6 8 10 11 2 6 8 10 1 7 9 11 0 8 10 12 1 9 11 13);G
                    (7 6 4 4 6 8 9 6 2 6 8 7 1 7 9 8 0 8 10 9 1 9 11);H
                    (9 8 6 4 4 6 7 8 6 2 6 9 7 1 7 10 8 0 8 11 9 1 9);I
                    (11 10 8 6 4 4 5 10 8 6 2 11 9 7 1 12 10 8 0 13 11 9 1);J
                    (6 5 5 7 9 11 12 3 7 9 11 2 8 10 12 1 9 11 13 0 10 12 14);K
                    (8 7 5 5 7 9 10 7 3 7 9 8 2 8 10 9 1 9 11 10 0 10 12);L
                    (10 9 7 5 5 7 8 9 7 3 7 10 8 2 8 11 9 1 9 12 10 0 10);M
                    (12 11 9 7 5 5 6 11 9 7 3 12 10 8 2 13 11 9 1 14 12 10 0)))

;;Start and end states. Not even trying to parse input from file today
(define input-p1
  (list #\. #\. #\. #\. #\. #\. #\.
        #\B #\C #\A #\B
        #\C #\D #\D #\A
        #\A #\B #\C #\D
        #\A #\B #\C #\D))

(define input-p2
  (list #\. #\. #\. #\. #\. #\. #\.
        #\B #\C #\A #\B
        #\D #\C #\B #\A
        #\D #\B #\A #\C
        #\C #\D #\D #\A))

(define goal
  (list #\. #\. #\. #\. #\. #\. #\.
        #\A #\B #\C #\D
        #\A #\B #\C #\D
        #\A #\B #\C #\D
        #\A #\B #\C #\D))

;Lists of points in each region
(define hall '(0 1 2 3 4 5 6))
(define room-a '(7 11 15 19))
(define room-b '(8 12 16 20))
(define room-c '(9 13 17 21))
(define room-d '(10 14 18 22))

;Movement cost for each type of amphipod
(define movement-costs (hash #\A 1 #\B 10 #\C 100 #\D 1000))

;;Display solution
(display "Part 1: ")
(cost-to-solve input-p1 0)
(display "Part 2: ")
(cost-to-solve input-p2 0)
