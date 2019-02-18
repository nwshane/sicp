(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

; The original good-enough? function
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
    (sqrt-iter 2.0 x))

; The new good-enough? function
(define (good-enough-new? guess x)
    (< (abs (/ guess (improve guess x)))
        1.000000001))

(define (sqrt-iter-new guess x)
  (if (good-enough-new? guess x)
      guess
      (sqrt-iter-new (improve guess x)
                 x)))

(define (sqrt-new x)
    (sqrt-iter-new 2.0 x))

; Generalized method for testing examples with
; both the original and new sqrt methods and
; evaluating which did a better job.
(define (test-example x)
    (begin
       (define sqrt-val (sqrt (square x)))
       (define sqrt-new-val (sqrt-new (square x)))
       (newline)
       (display "(sqrt ")
       (display x)
       (display "): ")
       (display sqrt-val)
       (newline)
       (display "(sqrt-new ")
       (display x)
       (display "): ")
       (display sqrt-new-val)
       (define sqrt-val-diff (abs (- sqrt-val x)))
       (define sqrt-new-val-diff (abs (- sqrt-new-val x)))
       (cond ((< sqrt-val-diff sqrt-new-val-diff) "sqrt wins")
             ((> sqrt-val-diff sqrt-new-val-diff) "sqrt-new wins")
             (else "tie"))
    )
)

(test-example 0.0000000004)
(test-example 0.001)
(test-example 43214432)
(test-example 400)
(test-example 5)
(test-example 1.3)
(test-example 0.1)
(test-example 0.9999)
(test-example 1.0001)
(test-example 1.0004)
(test-example 1.0005)
(test-example 1.0006)
(test-example 1.0007)
(test-example 1.0009)
(test-example 1.001)
(test-example 1.01)
(test-example 1.1)

; Question: Why is the original good-enough? function inaccurate
; for large numbers?
;
; ??? Not sure - it seems to be quite accurate!

; Question: Why is the original good-enough? function inaccurate
; for very small numbers?
;
; The original good-enough? function will fail
; to be accurate for small numbers, because the square
; root of a very small number will also be very small,
; and looking at the difference of the guess's square
; and the target number will not bring you very close
; to the real square root.


; CONCLUSIONS
;
; It seems like good-enough? is better for large numbers,
; and good-enough-new? is better for small numbers. However,
; I don't understand why sqrt-new always outputs 1 for
; large numbers, and I also don't understand why the textbook
; is implying that original sqrt doesn't work for large numbers.