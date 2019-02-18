(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.1))

(define (cbrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (cbrt-iter (improve guess x) x)))

(define (cbrt x) (cbrt-iter 1.0 x))

(cbrt (cube 4324))
(cbrt (cube 20))
(cbrt (cube 3))
(cbrt (cube 1.1))