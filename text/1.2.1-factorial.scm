(define (fact1 n)
    (if (= n 1)
        1
        (* n (fact1 (- n 1)))))

(fact1 10)

(define (fact-iter goal counter product)
    (if (> counter goal)
        product
        (fact-iter
            goal
            (+ counter 1)
            (* product counter))))

(define (fact2 n) (fact-iter n 1 1))

(fact2 10)