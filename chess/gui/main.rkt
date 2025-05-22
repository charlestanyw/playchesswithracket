#lang racket
(require racket/gui
         "../engine/board.rkt"
         "../engine/pieces.rkt"
         "../engine/game.rkt"
         "../engine/rules/core.rkt")

(define SQUARE-SIZE 60)
(define game (make-game-state))
(define selected-piece #f)
(define possible-moves '())

(define frame (new frame%
                   [label "Chess"]
                   [width (* 8 SQUARE-SIZE)]
                   [height (* 8 SQUARE-SIZE)]))

;; Draw the chessboard and pieces
(define (draw-board dc)
  ;; Draw chess board squares
  (for ([row (in-range 8)])
    (for ([col (in-range 8)])
      (define x (* col SQUARE-SIZE))
      (define y (* row SQUARE-SIZE))
      (send dc set-brush
            (if (even? (+ row col))
                "light gray"
                "white")
            'solid)
      (send dc draw-rectangle x y SQUARE-SIZE SQUARE-SIZE)

      ;; Highlight possible moves
      (when (member (cons row col) possible-moves)
        (send dc set-brush "light green" 'solid)
        (send dc draw-rectangle x y SQUARE-SIZE SQUARE-SIZE))))

  ;; Draw pieces
  (for ([row (in-range 8)])
    (for ([col (in-range 8)])
      (define p (board-ref (game-state-board game) row col))
      (when p
        (define x (+ (* col SQUARE-SIZE) 15))
        (define y (+ (* row SQUARE-SIZE) 10))
        (send dc set-font (make-object font% 36 'default))
        (send dc draw-text (string (piece-symbol p)) x y)))))

;; Create canvas-
(define canvas
  (new canvas%
       [parent frame]
       [paint-callback (λ (_ dc) (draw-board dc))]))

;; Handle mouse click
(define (handle-click row col)
  (define piece (get-piece-at game row col))

  (cond
    ;; Selecting a piece
    [(and (not selected-piece) piece (eq? (piece-color piece) (current-player game)))
     (set! selected-piece (cons row col))
     (set! possible-moves (get-valid-moves game row col))
     (send canvas refresh)]

    ;; Moving a selected piece
    [(and selected-piece (member (cons row col) possible-moves))
     (move-piece! game (car selected-piece) (cdr selected-piece) row col)
     (set! selected-piece #f)
     (set! possible-moves '())
     (send canvas refresh)]

    ;; Canceling selection
    [else
     (set! selected-piece #f)
     (set! possible-moves '())
     (send canvas refresh)]))

;; Connect mouse events to canvas
(send canvas on-event
      (λ (event)
        (when (send event button-down? 'left)
          (define x (send event get-x))
          (define y (send event get-y))
          (define row (quotient y SQUARE-SIZE))
          (define col (quotient x SQUARE-SIZE))
          (when (and (<= 0 row 7) (<= 0 col 7))
            (handle-click row col)))))

;; Utility functions
(define (current-player g) (game-state-turn g))
(define (get-piece-at g row col) (board-ref (game-state-board g) row col))

(define (get-valid-moves gs row col)
  (define piece (board-ref (game-state-board gs) row col))
  (define from (cons row col))

  (filter
   (λ (to) (valid-move? (game-state-board gs) piece from to))
   (append-map
    (λ (r) (map (λ (c) (cons r c)) (range 8)))
    (range 8))))

;; Show the window
(send frame show #t)
