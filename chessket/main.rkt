#lang racket
(require "gui/main.rkt"
         (for-syntax syntax/parse))

(provide (except-out (all-from-out racket)
                     #%module-begin)
         (all-from-out racket))


(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #`(#%module-begin
        ;; Start the game
        (println "Welcome to Racket Chess!\n")
        form ...)]))

