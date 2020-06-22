#lang slideshow

;; Dimensions of the Connect4 boards
(define ROWS 6)
(define COLUMNS 7)

;; Player info
(define NO-PLAYER 0)
(define MAX-PLAYER 1)
(define MIN-PLAYER 2)

;; Game position struct
(struct node (board player alpha beta) #:transparent)

;; Returns the starting node of the game
(define (start-node)
  (node (build-list (* ROWS COLUMNS) (lambda (_) NO-PLAYER))
        MAX-PLAYER
        -inf.0
        +inf.0))

;; Params for displaying board
(define DIA 15)
(define EMPTY-CIRCLE (circle DIA))
(define MAX-PLAYER-CIRCLE (disk DIA #:color "Red"))
(define MIN-PLAYER-CIRCLE (disk DIA #:color "Blue"))
(define HSPACE 2)
(define VSPACE 2)

;; Pretty-prints out the node
(define (print-node node)
  (let ([board (node-board node)])
    (apply hc-append HSPACE
           (build-list COLUMNS (lambda (col)
                                 (apply vc-append VSPACE
                                        (build-list ROWS (lambda (row)
                                                           (let ([tile (list-ref board (+ (* row COLUMNS) col))])
                                                             (cond [(equal? tile NO-PLAYER) EMPTY-CIRCLE]
                                                                   [(equal? tile MAX-PLAYER) MAX-PLAYER-CIRCLE]
                                                                   [(equal? tile MIN-PLAYER) MIN-PLAYER-CIRCLE]))))))))))
