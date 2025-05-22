#lang racket

(provide (all-defined-out))
(provide piece piece? piece-type piece-color) 

(struct piece (type color) #:transparent)
(define BOARD-SIZE 8)

(define (board-ref board row col)
  (and (< -1 row BOARD-SIZE)
       (< -1 col BOARD-SIZE)
       (vector-ref (vector-ref board row) col)))

(define (make-empty-board)
  (build-vector BOARD-SIZE (λ (_) (make-vector BOARD-SIZE #f))))

(define (board-set! board row col val)
  (vector-set! (vector-ref board row) col val))

(define (make-standard-board)
  (vector
   ;; Black pieces
   (vector (piece 'rook 'black) (piece 'knight 'black) (piece 'bishop 'black)
           (piece 'queen 'black) (piece 'king 'black) (piece 'bishop 'black)
           (piece 'knight 'black) (piece 'rook 'black))
   (build-vector BOARD-SIZE (λ (_) (piece 'pawn 'black)))
   ;; Empty rows
   (make-vector BOARD-SIZE #f)
   (make-vector BOARD-SIZE #f)
   (make-vector BOARD-SIZE #f)
   (make-vector BOARD-SIZE #f)
   ;; White pieces
   (build-vector BOARD-SIZE (λ (_) (piece 'pawn 'white)))
   (vector (piece 'rook 'white) (piece 'knight 'white) (piece 'bishop 'white)
           (piece 'queen 'white) (piece 'king 'white) (piece 'bishop 'white)
           (piece 'knight 'white) (piece 'rook 'white))))