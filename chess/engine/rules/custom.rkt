#lang racket
(require "../core.rkt")

(provide (all-defined-out))

;; DSL definitions for custom pieces would go here
(define-piece pawn
  [movement (λ (board from) ...)]
  [capture (λ (board from) ...)])