(define square (lambda (x) (* x x)))

(define cube (lambda (x) (* x x x)))

(define log2 (lambda (x) (/ (log x) (log 2))))

(define log10 (lambda (x) (/ (log x) (log 10))))

(define sqrt (lambda (x) (expt x 0.5)))

(define Pi (/ 355 113))

(define E (/ 49171 18089))

(define rad (lambda (x) (/ (* Pi x) 180)))

(define deg (lambda (x) (/ (* 180 x) Pi)))

; continued fraction, (cfract 1 n)
(define cfract
  (lambda (x y)
    (cond
      ((< x y) (/ x (+ 1 (cfract (+ x 1) y))))
      (T y))))


; factorial
(define fact
  (lambda (x)
    (cond
      ((eq 0 x) 1)
      (T (* x (fact (- x 1)))))))


; Fibonacci
(define fib
  (lambda (x)
    (cond
      ((< x 2) x)
      (T (+ (fib (- x 1)) (fib (- x 2)))))))
