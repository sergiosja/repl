(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))

(define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (list-length lst) (if (null? lst) 0 (+ 1 (list-length (cdr lst)))))

(define (square x) (* x x))