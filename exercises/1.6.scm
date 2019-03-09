(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; The original sqrt-iter function with normal if
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

; The new sqrt-iter function with custom implementation of new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-new guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                        x)))

(define (sqrt-new x)
    (sqrt-iter-new 1.0 x))

(sqrt 9)
(sqrt-new 9)
(sqrt (* 100 100))
(sqrt-new (* 100 100))

; sqrt-new seems to work just as well as sqrt! Not sure what to make
; of this question...