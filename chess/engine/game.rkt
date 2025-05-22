#lang racket
(provide (all-defined-out))

(require "board.rkt"
         "pieces.rkt"
         "rules/core.rkt")

(struct game-state (board turn selected-pos game-over?) #:mutable)

(define (make-game-state [board (make-standard-board)])
  (game-state board 'white #f #f))

(define (check-game-over! gs)
  (define kings-alive (for*/fold ([kings '()])
                        ([(row i) (in-indexed (game-state-board gs))]
                         [(piece j) (in-indexed row)]
                         #:when (and piece (eq? (piece-type piece) 'king)))
                       (cons (piece-color piece) kings)))
  (unless (and (member 'white kings-alive) (member 'black kings-alive))
    (set-game-state-game-over?! gs #t)))

(define (move-piece! gs from-row from-col to-row to-col)
  (define board (game-state-board gs))
  (define piece (board-ref board from-row from-col))
  
  (when (and piece (valid-move? board piece (cons from-row from-col) (cons to-row to-col)))
    (board-set! board to-row to-col piece)
    (board-set! board from-row from-col #f)
    (set-game-state-turn! gs (if (eq? (game-state-turn gs) 'white) 'black 'white))
    (check-game-over! gs)))

(define (get-piece-at game-state row col)
  (board-ref (game-state-board game-state) row col))

(define (current-player game-state)
  (game-state-turn game-state))