#lang slideshow

; most of the required packages are not needed
; the only one that is really needed is for pict-convertible? which is textpict/mrpict

(require scheme/class
         scheme/unit
         scheme/file
         racket/draw
         racket/match
         texpict/mrpict
         texpict/utils
         scheme/math
         "sig.ss"
         "private/utils.ss"
         "private/aspect.rkt")

; originally thought it will be helpful, but realized it might be useless
#;(define item-contract (() (#:bullet pict?
                           #:aspect aspect?
                           #:width real?
                           #:gap-size real?
                           #:align (or/c 'left 'center 'right)
                           #:fill? any/c
                           #:decode? any/c)
                          #:rest elem/c
                          . ->* . pict?))

(define client-ws #hasheq())
(define client-hs #hasheq())

(define elem/c (flat-rec-contract elem/c (or/c string? #t (listof elem/c)))) ; pict-convertible? is replaced with #t

(define current-para-widths
        (for/hasheq ([(aspect client-w) (in-hash client-ws)])
          (values aspect (make-parameter client-w))))

(define (check-aspect who v)
    (unless (aspect? v)
      (raise-argument-error who "aspect?" v)))

(define (shift-no-sep l)
	(let loop ([l 
		    ;; Flatten l, first:
		    (let loop ([l l])
		      (cond
		       [(null? l) null]
		       [(pair? (car l)) (append (loop (car l)) (loop (cdr l)))]
		       [else (cons (car l) (loop (cdr l)))]))]
		   [a null])
	  ;; Combine strings:
	  (cond
	   [(null? l) (reverse a)]
	   [(null? a) (loop (cdr l) (list (car l)))]
	   [(and (string? (car l)) 
		 (regexp-match #rx"^[-',. :;?!)\U201D\U2019]" (car l)))
	    (let ([m (regexp-match #rx"^([^ ]*) (.*)$" (car l))])
	      (if m
                  (if (string? (car a))
		      (loop (cdr l)
			    (list* (caddr m)
				   (string-append (car a) (cadr m))
				   (cdr a)))
		      (loop (cdr l)
			    (list* (caddr m)
				   (hbl-append (car a) (t (cadr m)))
				   (cdr a))))
		  (if (string? (car a))
		      (loop (cdr l)
			    (cons (string-append (car a) (car l))
				  (cdr a)))
		      (loop (cdr l)
			    (cons (hbl-append (car a) (t (car l)))
				  (cdr a))))))]
	   [else (loop (cdr l) (cons (car l) a))])))

(define (para*/align v-append w . s)
  (define space (t " "))
  (let loop ([pre #f][s (shift-no-sep s)][rest null])
    (cond
      [(null? s)
       (if (null? rest)
           (or pre (blank))
           (loop pre (car rest) (cdr rest)))]
      [(list? s) (loop pre (car s) (append (cdr s) rest))]
      [else
       (let* ([p (if (string? s) (t s) s)])
         (cond
           [(< (+ (if pre (pict-width pre) 0)
                  (if pre (pict-width space) 0)
                  (pict-width p)) 
               w)
            ;; small enough
            (loop (if pre 
                      (hbl-append pre space p) 
                      p)
                  rest null)]
           [(and (string? s) (regexp-match "(.*) (.*)" s))
            ;; can break on string
            => (lambda (m)
                 (loop pre
                       (cadr m) 
                       (cons
                        (caddr m)
                        rest)))]
           [(not pre)
            (if (null? rest)
                p
                (v-append
                 (current-line-sep)
                 p
                 (loop #f rest null)))]
           [else
            (v-append
             (current-line-sep)
             pre
             (loop p rest null))]))])))

      (define (decode s)
        (let loop ([s s])
          (cond
           [(list? s) 
            (map 
             decode
             ;; Remove "\n", and also cancel extra spaces after "\n":
             (let loop ([s s])
               (cond
                [(null? s) null]
                [(equal? (car s) "\n")
                 (let nloop ([s (cdr s)])
                   (if (and (pair? s)
                            (string? (car s)))
                       (let ([a (regexp-replace #rx"^ +" (car s) "")])
                         (if (string=? a "")
                             (nloop (cdr s))
                             (loop (cons a (cdr s)))))
                       (loop s)))]
                [else (cons (car s) (loop (cdr s)))])))]
           [(not (string? s)) s]
           [(regexp-match-positions #rx"---" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u2014"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #rx"--" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u2013"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #px"``" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u201C"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #px"''" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u201D"
                                (loop (substring s (cdar m)))))]
           [(regexp-match-positions #rx"'" s)
            => (lambda (m)
                 (string-append (loop (substring s 0 (caar m)))
                                "\u2019"
                                (loop (substring s (cdar m)))))]           
           [else s])))

(define para/kw
  (let ([para (lambda (#:aspect [aspect #f]
                       #:width [width ((hash-ref current-para-widths aspect))]
                       #:align [align 'left]
                       #:fill? [fill? #t]
                       #:decode? [decode? #t]
                       . s)
                (check-aspect 'para aspect)
                (let ([p (para*/align (case align
                                        [(right) vr-append]
                                        [(center) vc-append]
                                        [else vl-append])
                                      width
                                      (if decode?
                                          (decode s)
                                          s))])
                  (if fill?
                      ((case align
                         [(right) rtl-superimpose]
                         [(center) ctl-superimpose]
                         [else ltl-superimpose])
                       (blank width 0)
                       p)
                      p)))])
    para))