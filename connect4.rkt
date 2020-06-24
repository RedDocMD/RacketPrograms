#lang racket/gui
(require pict)

;; Dimensions of the Connect4 boards
(define ROWS 6)
(define COLUMNS 7)

;; Player info
(define NO-PLAYER 0)
(define MAX-PLAYER 1)
(define MIN-PLAYER 2)

;; Game position struct
(struct node (board player [alpha #:mutable] [beta #:mutable]) #:transparent)

;; Game state struct
(struct state ([curr-node #:mutable] [move# #:mutable]))

;; Returns the starting node of the game
(define (start-node)
  (node (build-list (* ROWS COLUMNS) (lambda (_) NO-PLAYER))
        MAX-PLAYER
        -inf.0
        +inf.0))

;; Returns the start state of the game
(define (start-state)
  (state (start-node) 0))

;; Params for displaying board
(define DIA 50)
(define EMPTY-CIRCLE (circle DIA))
(define MAX-PLAYER-CIRCLE (disk DIA #:color "Red"))
(define MIN-PLAYER-CIRCLE (disk DIA #:color "Blue"))
(define HSPACE 2)
(define VSPACE 2)

;; Generates a pict for the node
(define (pict-node node)
  (let ([board (node-board node)])
    (apply hc-append HSPACE
           (build-list COLUMNS (lambda (col)
                                 (apply vc-append VSPACE
                                        (build-list ROWS (lambda (row)
                                                           (let ([tile (list-ref board (+ (* row COLUMNS) col))])
                                                             (cond [(equal? tile NO-PLAYER) EMPTY-CIRCLE]
                                                                   [(equal? tile MAX-PLAYER) MAX-PLAYER-CIRCLE]
                                                                   [(equal? tile MIN-PLAYER) MIN-PLAYER-CIRCLE]))))))))))
;; Returns the said column as a list
(define (get-column tiles column)
  (build-list ROWS (lambda (row) (list-ref tiles (+ (* row COLUMNS) column)))))

;; Returns the said row as a list
(define (get-row tiles row)
  (build-list COLUMNS (lambda (column) (list-ref tiles (+ (* row COLUMNS) column)))))

;; Returns the number of chains of val of length len in tiles 
(define (chain-count val len tiles)
  (let ([start-lim (+ (length tiles) (- len) 1)])
    (count identity (map (lambda (pos)
                           (equal? (count (lambda (num) (equal? num val)) (take (list-tail tiles pos) len)) len))
                         (stream->list (in-range start-lim))))))
  

;; Makes a move and returns the next node or #f if illegal move
(define (make-move curr-node col)
  (let* ([board (node-board curr-node)]
         [next-player (if (equal? (node-player curr-node) MAX-PLAYER)
                          MIN-PLAYER
                          MAX-PLAYER)]
         [curr-column (get-column board col)]
         [ins-row -2])
    (for ([i (in-range 1 ROWS)])
      (when (and (not (equal? (list-ref curr-column i) NO-PLAYER)) (< ins-row 0))
        (set! ins-row (sub1 i))))
    (when (equal? ins-row -2) (set! ins-row (sub1 ROWS)))
    (if (or (< ins-row 0) (not (equal? (list-ref curr-column ins-row) NO-PLAYER))) 
        #f
        (node (list-set board (+ (* ins-row COLUMNS) col) (node-player curr-node))
              next-player
              (node-alpha curr-node)
              (node-beta curr-node)))))



;; The mutable state of the game
(define game-state (start-state))


(define frame (new frame% 
                   [label "Connect4 Game"]
                   [min-width 500]
                   [min-height 500]))

(define vertical-panel (new vertical-pane% [parent frame] [alignment '(center center)]))

(define canvas (new canvas% 
                    [parent vertical-panel]
                    [paint-callback
                     (lambda (canvas dc)
                       (draw-pict (pict-node (state-curr-node game-state)) dc 0 0))]))

(define input-panel (new horizontal-pane% 
                         [parent vertical-panel]
                         [alignment '(center top)]))

(define move-button (new button% 
                         [parent input-panel]
                         [label "Make Move"]
                         [callback (lambda (button event)
                                     (let ([next-node (make-move (state-curr-node game-state) (send col-slider get-value))])
                                       (define (make-human-move)
                                         (set-state-curr-node! game-state next-node)
                                         (set-state-move#! game-state (add1 (state-move# game-state))))
                                       (if next-node
                                           (make-human-move)
                                           (println "Invalid move")))
                                     (send canvas refresh))]))

(define col-slider (new slider% [parent input-panel]
                        [label "Input column"]
                        [min-value 0]
                        [max-value (sub1 COLUMNS)]))

(send frame show #t)