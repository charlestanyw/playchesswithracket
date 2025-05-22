#lang racket
(provide (all-defined-out))
(require "../board.rkt")

;; ==================== PATH CHECKING ====================
(define (clear-path? board from to)
  (let* ([fr (car from)]
         [fc (cdr from)]
         [tr (car to)]
         [tc (cdr to)]
         [dr (sgn (- tr fr))]
         [dc (sgn (- tc fc))])
    ;; Check path is straight or diagonal
    (unless (or (zero? dr) (zero? dc) (= (abs (- tr fr)) (abs (- tc fc))))
      (error 'clear-path? "invalid path"))

    ;; Check for obstructions
    (let loop ([r (+ fr dr)] [c (+ fc dc)])
      (cond
        [(and (= r tr) (= c tc)) #t]
        [(board-ref board r c) #f]
        [else (loop (+ r dr) (+ c dc))]))))

;; ==================== MOVEMENT RULES ====================
(define (valid-move? board piece from to)
  (let* ([from-row (car from)]
         [from-col (cdr from)]
         [to-row (car to)]
         [to-col (cdr to)]
         [target (board-ref board to-row to-col)])
    (and
     (not (and target (eq? (piece-color target) (piece-color piece)))) 
     (case (piece-type piece)
       [(pawn) (valid-pawn-move? board piece from to)]
       [(knight) (valid-knight-move? board piece from to)]
       [(bishop) (valid-bishop-move? board piece from to)]
       [(rook) (valid-rook-move? board piece from to)]
       [(queen) (valid-queen-move? board piece from to)]
       [(king) (valid-king-move? board piece from to)]
       [else #f]))))

;; Individual piece movement rules
(define (valid-pawn-move? board piece from to)
  (let* ([from-row (car from)]
         [from-col (cdr from)]
         [to-row (car to)]
         [to-col (cdr to)]
         [direction (if (eq? (piece-color piece) 'white) -1 1)]
         [target (board-ref board to-row to-col)])
    (cond
      ;; Forward move
      [(= from-col to-col)
       (and (not target)
            (or (= to-row (+ from-row direction))
                (and (= to-row (+ from-row (* 2 direction)))
                     (or (and (eq? (piece-color piece) 'white) (= from-row 6))
                         (and (eq? (piece-color piece) 'black) (= from-row 1))))))]
      ;; Diagonal capture
      [(and (= (abs (- to-col from-col)) 1)
            (= to-row (+ from-row direction))
       (and target
            (not (eq? (piece-color target) (piece-color piece)))))]
      [else #f])))

;; Other piece movement rules would follow the same pattern
(define (valid-knight-move? board piece from to)
  (let ([dr (abs (- (car to) (car from)))]
        [dc (abs (- (cdr to) (cdr from)))])
    (member (list dr dc) '((2 1) (1 2)))))

(define (valid-bishop-move? board piece from to)
  (and (= (abs (- (car to) (car from))) (abs (- (cdr to) (cdr from))))
       (clear-path? board from to)))

(define (valid-rook-move? board piece from to)
  (and (or (= (car to) (car from)) (= (cdr to) (cdr from)))
       (clear-path? board from to)))

(define (valid-queen-move? board piece from to)
  (or (valid-bishop-move? board piece from to)
      (valid-rook-move? board piece from to)))

(define (valid-king-move? board piece from to)
  (and (<= (abs (- (car to) (car from))) 1)
       (<= (abs (- (cdr to) (cdr from))) 1)))