#lang racket
(provide (all-defined-out))
(require "board.rkt"
         "pieces.rkt"
         "rules/core.rkt"
         racket/gui)

(struct game-state (board turn selected-pos game-over? first-clicked-pos) #:mutable)

(define (make-game-state [board (make-standard-board)])
  (game-state board 'white #f #f #f))

(define (check-game-over! gs)
  (define kings-alive (for*/fold ([kings '()])
                        ([(row i) (in-indexed (game-state-board gs))]
                         [(piece j) (in-indexed row)]
                         #:when (and piece (eq? (piece-type piece) 'king)))
                       (cons (piece-color piece) kings)))
  (unless (and (member 'white kings-alive) (member 'black kings-alive))
    (set-game-state-game-over?! gs #t)
    (message-box "Game Over" 
                (if (member 'white kings-alive) "White wins!" "Black wins!")
                #f '(ok))))

(define (move-piece! gs from-row from-col to-row to-col)
  (define board (game-state-board gs))
  (define piece (board-ref board from-row from-col))
  
  (when piece
    (board-set! board to-row to-col piece)
    (board-set! board from-row from-col #f)
    (set-game-state-turn! gs (if (eq? (game-state-turn gs) 'white) 'black 'white))
    (check-game-over! gs)))