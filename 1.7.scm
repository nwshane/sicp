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
    (sqrt-iter 1.0 x))

; The new good-enough? function
(define (good-enough-new? guess x)
    (< (abs (/ guess (improve guess x)))
        1.00001))

(define (sqrt-iter-new guess x)
  (if (good-enough-new? guess x)
      guess
      (sqrt-iter-new (improve guess x)
                 x)))

(define (sqrt-new x)
    (sqrt-iter-new 1.0 x))

; COULD define a way to test these examples... would
(define (test-example x)
    (begin
       (newline)
       (display "sqrt method: ")
       (display (sqrt (* x x)))
       (newline)
       (display "sqrt-new method: ")
       (display (sqrt-new (* x x)))
    )
)


; LARGE NUMBERS
;
; Question: Why is the original good-enough? function inaccurate
; for large numbers?
;
; ??? Not sure - it seems to be quite accurate!

; SMALL NUMBERS
;
; Question: Why is the original good-enough? function inaccurate
; for very small numbers?
;
; The original good-enough? function will fail
; to be accurate for small numbers, because the square
; root of a very small number will also be very small,
; and looking at the difference of the guess's square
; and the target number will not bring you very close
; to the real square root.
;
; EXAMPLE 1
(test-example 0.0000000004)

; EXAMPLE 2
(test-example 0.001)

; EXAMPLE 3
(test-example 43214432)

; EXAMPLE 4
(test-example 400)

; Example 5
(test-example 5)

; Example 6
(test-example 1.3)

; Example 7
(test-example 0.1)



; CONCLUSIONS
;
; It seems like good-enough? is better for large numbers,
; and good-enough-new? is better for small numbers.