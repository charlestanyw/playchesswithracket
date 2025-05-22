#lang racket
(provide (all-defined-out))

(require "rules/core.rkt")
(require "board.rkt") 

(define (piece-symbol p)
  (match (list (piece-type p) (piece-color p))
    [(list 'king   'white) #\♔]
    [(list 'queen  'white) #\♕]
    [(list 'rook   'white) #\♖]
    [(list 'bishop 'white) #\♗]
    [(list 'knight 'white) #\♘]
    [(list 'pawn   'white) #\♙]
    [(list 'king   'black) #\♚]
    [(list 'queen  'black) #\♛]
    [(list 'rook   'black) #\♜]
    [(list 'bishop 'black) #\♝]
    [(list 'knight 'black) #\♞]
    [(list 'pawn   'black) #\♟]
    [else #\?]))