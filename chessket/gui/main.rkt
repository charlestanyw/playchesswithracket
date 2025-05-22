#lang racket
;(require reprovide/reprovide)

(require racket/gui
         "../engine/board.rkt"
         "../engine/pieces.rkt"
         "../engine/game.rkt"
         "../engine/rules/core.rkt")

(provide (all-defined-out))

(define SQUARE-SIZE 60)
(define game (make-game-state))

(define frame (new frame% [label "Chess"] [width (* 8 SQUARE-SIZE)] [height (* 8 SQUARE-SIZE)]))

(define chess-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (and (send event button-down? 'left) (not (game-state-game-over? game)))
        (define x (send event get-x))
        (define y (send event get-y))
        (define row (quotient y SQUARE-SIZE))
        (define col (quotient x SQUARE-SIZE))
        (when (and (<= 0 row 7) (<= 0 col 7))
          (handle-click row col))))))

(define (draw-board dc)
  ;; Draw chess board squares
  (for ([row (in-range 8)])
    (for ([col (in-range 8)])
      (define x (* col SQUARE-SIZE))
      (define y (* row SQUARE-SIZE))
      (send dc set-brush (if (even? (+ row col)) "light gray" "white") 'solid)
      (send dc draw-rectangle x y SQUARE-SIZE SQUARE-SIZE)))

  ;; Highlighting selected cell
  (define selected (game-state-selected-pos game))
  (when selected
    (define x (* (cdr selected) SQUARE-SIZE))
    (define y (* (car selected) SQUARE-SIZE))
    (send dc set-pen "red" 3 'solid)
    (send dc set-brush (send the-brush-list find-or-create-brush "red" 'transparent))
    (send dc draw-rectangle x y SQUARE-SIZE SQUARE-SIZE)
    (send dc set-pen "black" 1 'solid))

  ;; Draw pieces
  (for ([row (in-range 8)])
    (for ([col (in-range 8)])
      (define p (board-ref (game-state-board game) row col))
      (when p
        (define x (+ (* col SQUARE-SIZE) 20))
        (define y (+ (* row SQUARE-SIZE) 15))
        (send dc set-font (make-object font% 28 'modern 'normal 'bold))
        (send dc draw-text (string (piece-symbol p)) x y)))))


;; Handling mouse clicks
(define (handle-click row col)
  (define clicked-pos (cons row col))
  (define clicked-piece (board-ref (game-state-board game) row col))
  (define first-clicked (game-state-first-clicked-pos game))

  (cond
    [(not first-clicked)
     (when (and clicked-piece (eq? (piece-color clicked-piece) (game-state-turn game)))
       (set-game-state-first-clicked-pos! game clicked-pos)
       (set-game-state-selected-pos! game clicked-pos))]
    
    [else
     (define from first-clicked)
     (define moving-piece (board-ref (game-state-board game) (car from) (cdr from)))
     
     (when (and moving-piece (valid-move? (game-state-board game) moving-piece from clicked-pos))
       (move-piece! game (car from) (cdr from) row col))
     
     (set-game-state-first-clicked-pos! game #f)
     (set-game-state-selected-pos! game #f)])
  
  (send canvas refresh))


(define canvas (new chess-canvas% [parent frame] [paint-callback (λ (_ dc) (draw-board dc))]))
(send frame show #t)

