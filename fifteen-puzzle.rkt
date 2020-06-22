#lang racket
;; This struct denotes a Fifteen Puzzle game position
;; tiles - A flattened list of the game board, flattened row-major
(struct board (tiles depth) #:transparent)

;; Dimension of the board
(define SIZE 4)

;; Start position of the game
(define start-second (board '(15 14 1 6 9 11 4 12 0 10 7 3 13 8 5 2) 0))

;; This function given an action and a game board, finds the next board
;; or returns #f if the given action produces an invalid board
;; curr-node - Game board to generate next move from
;; action - Function that given the position of 0 in this board gives the next
(define (perform-action curr-node action)
  (let* ([curr-tiles (board-tiles curr-node)]
         [curr-zero (index-of curr-tiles 0)]
         [next-zero (action curr-zero)])
    (if (equal? next-zero -1)
        #f
        (let* ([first-idx (min curr-zero next-zero)]
               [second-idx (max curr-zero next-zero)]
               [first-val (list-ref curr-tiles first-idx)]
               [second-val (list-ref curr-tiles second-idx)]
               [next-tiles (list-set (list-set curr-tiles first-idx second-val) second-idx first-val)])
          (board next-tiles (add1 (board-depth curr-node)))))))

;; Returns the prefix list [0:idx)
(define (head-list lst idx)
  (let-values ([(head tail) (split-at lst idx)])
    head))

;; Returns the suffix list (idx:len(lst)]
(define (tail-list lst idx)
  (let-values ([(head tail) (split-at lst (add1 idx))])
    tail))

;; Move the 0 vertically by 1 position
;; op - add1 for down and sub1 for up
;; zero-pos current position of the 0
(define (move-vertically zero-pos op)
  (let* ([i (quotient zero-pos SIZE)]
         [j (remainder zero-pos SIZE)]
         [ii (op i)])
    (if (and (>= ii 0) (< ii SIZE))
        (+ (* ii SIZE) j)
        -1)))

;; Move the 0 horizontally by 1 position
;; op - add1 for right and sub1 for left
;; zero-pos current position of the 0
(define (move-horizontally zero-pos op)
  (let* ([i (quotient zero-pos SIZE)]
         [j (remainder zero-pos SIZE)]
         [jj (op j)])
    (if (and (>= jj 0) (< jj SIZE))
        (+ (* i SIZE) jj)
        -1)))


;; Move the 0 up by 1 position
;; zero-pos current position of the 0
(define (move-up zero-pos)
  (move-vertically zero-pos sub1))

;; Move the 0 down by 1 position
;; zero-pos current position of the 0
(define (move-down zero-pos)
  (move-vertically zero-pos add1))

;; Move the 0 left by 1 position
;; zero-pos current position of the 0
(define (move-left zero-pos)
  (move-horizontally zero-pos sub1))

;; Move the 0 right by 1 position
;; zero-pos current position of the 0
(define (move-right zero-pos)
  (move-horizontally zero-pos add1))

;; List of possible actions
(define ACTIONS (list (cons move-up "UP") (cons move-down "DOWN") (cons move-left "LEFT") (cons move-right "RIGHT")))

;; Prints a board
;; curr-board - board to print
(define (print-board curr-board)
  (let ([curr-tiles (board-tiles curr-board)])
    (for ([n curr-tiles]
          [i (in-range (sqr SIZE))])
      (if (equal? (remainder i SIZE) (sub1 SIZE))
          (display (string-append (number->string n) "\n"))
          (display (string-append (number->string n) " "))))))

;; Finds Manhattan distance heuristic for board
;; curr-node - board to calculate heurisitc of
(define (summed-manhattan curr-node)
  (foldl (lambda (idx num result)
           (let ([i (quotient idx SIZE)]
                 [j (remainder idx SIZE)]
                 [ii (quotient (sub1 num) SIZE)]
                 [jj (remainder (sub1 num) SIZE)])
             (if (equal? num 0)
                 (+ result (abs (- i (sub1 SIZE))) (abs (- j (sub1 SIZE))))
                 (+ result (abs (- i ii)) (abs (- j jj))))))
         0 (board-tiles curr-node) (stream->list (sequence->stream (in-range (sqr SIZE))))))

;; Recursive part of IDA*
;; node - current node expanding
;; limit - f limit
;; heuristic - heuristic function used
(define (recursive-ida* node limit heuristic)
  (cond
    [(goal? node)
     (values 'SUCCESS node limit)]
    [(> (node-f-value heuristic node) limit)
     (values 'CUTOFF node (node-f-value heuristic node))]
    [else
     (let ([cutoff #f]
           [new-cutoff +inf.0]
           [exit-early #f]
           [new-goal #f])
       (for ([action-pair ACTIONS]
             #:break exit-early)
         (let* ([action (car action-pair)]
                [command (cdr action-pair)]
                [next-node (perform-action node action)])
           (when next-node
             (let-values ([(state goal new-limit) (recursive-ida* next-node limit heuristic)])
               (cond
                 [(equal? state 'SUCCESS)
                  (set! exit-early #t)
                  (set! new-goal goal)]
                 [(equal? state 'CUTOFF)
                  (set! cutoff #t)
                  (set! new-cutoff (min new-cutoff new-limit))])))))
       (cond
         [cutoff
          (values 'CUTOFF node new-cutoff)]
         [exit-early
          (values 'SUCCESS new-goal limit)]
         [else
          (values 'FAILURE node limit)]))]))

;; Returns f value of node
(define (node-f-value heuristic node)
        (+ (board-depth node) (heuristic node)))

;; Tiles in the goal state
(define GOAL_TILES '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

;; Checks whether the given node is a goal node
;; node - board to check
(define (goal? node)
  (equal? GOAL_TILES (board-tiles node)))

;; Perfoms IDA* search for the given start state and heuristic
;; node - current node expanding
;; heuristic - heuristic function used
(define (ida* start-node heuristic)
  (let ([f-limit (heuristic start-node)]
        [solved #f])
    (for ([i (in-naturals)]
          #:break solved)
      (let-values ([(state goal new-limit) (recursive-ida* start-node f-limit heuristic)])
        (cond
          [(equal? state 'SUCCESS)
           (set! solved #t)
           (display "Solved")
           (newline)
           (print-board goal)]
          [(equal? state 'FAILURE)
           (set! solved #t)
           (display "Impossible to solve")]
          [(equal? state 'CUTOFF)
           (set! f-limit new-limit)])))))

(define (start)
  (ida* start-second summed-manhattan))

(start)
              
          
    
