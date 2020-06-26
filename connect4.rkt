#lang racket/gui
(require pict)


;; Dimensions of the Connect4 boards
(define ROWS 6)
(define COLUMNS 7)
(define CHAIN 4)


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


;; List of coordinates for chain starting at (i, j) and in direction dir
(define (list-coords-in-dir i j dir)
  (define (rec-list i j dir)
    (let ([x (car dir)]
          [y (cdr dir)])
      (if (or (< i 0) (< j 0) (>= i ROWS) (>= j COLUMNS))
          empty
          (append (list (cons i j)) (rec-list (+ i x) (+ j y) dir)))))
  (rec-list i j dir))


;; Get chains in some direction
(define (get-chains i j dir tiles)
  (let ([coords (list-coords-in-dir i j dir)])
    (map (lambda (coord)
           (let* ([i (car coord)]
                  [j (cdr coord)]
                  [idx (+ (* i COLUMNS) j)])
             (list-ref tiles idx))))))


;; Get chains in the right diagonal direction
(define (get-right-diagonal-chains i j tiles)
  (get-chains i j (cons -1 +1) tiles))


;; Get chains in the left diagonal direction
(define (get-left-diagonal-chains i j tiles)
  (get-chains i j (cons +1 -1) tiles))  
  

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

;; Returns all consecutive chains of tiles
(define (get-all-chains tiles)
  (let* ([row-ids (stream->list (in-range ROWS))]
         [column-ids (stream->list (in-range COLUMNS))]
         [position-ids (for*/list ([i row-ids]
                                   [j column-ids])
                         (cons i j))])
    (let ([rows (map (lambda (row) (get-row tiles row)) row-ids)]
          [columns (map (lambda (column) (get-column tiles column)) column-ids)]
          [right-diagonals (map (lambda (position) (get-right-diagonal-chains (car position) (cdr position))) position-ids)]
          [left-diagonals (map (lambda (position) (get-left-diagonal-chains (car position) (cdr position))) position-ids)])
      (list rows columns right-diagonals left-diagonals))))


;; Look for chains of length CHAIN
(define (find-win-chain chains player)
  (ormap (lambda (tile-list)
           (ormap (lambda (chain) (> (chain-count player CHAIN chain) 0)) tile-list))
         chains))


;; Returns no. of chains of length len
(define (tot-chain-count player len chains)
  (apply + (map (lambda (tile-list)
                  (apply + (map (lambda (chain) (chain-count player len chain)) tile-list)))
                chains)))


;; Static evaluator for a node
(define (static-evaluator curr-node)
  (let ([tiles (node-board curr-node)]
        [player (node-player curr-node)])
    (let* ([chains (get-all-chains tiles)]
           [value (cond [(find-win-chain chains player) 10000]
                        [(> (tot-chain-count player 3 chains) 0) 5000]
                        [(> (tot-chain-count player 2 chains) 0) 1000]
                        [(> (tot-chain-count player 1 chains) 0) 100])])
      (if (equal? player MAX-PLAYER)
          value
          (- value)))))


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