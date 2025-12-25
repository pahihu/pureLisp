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

;; vector operations
; V op x
(define vopx
  (lambda (op x y)
    (cond
      ((atom x) NIL)
      (T (cons (op (car x) y)
               (vopx op (cdr x) y))))))

(define vopv
  (lambda (op x y)
    (cond
      ((and (atom x) (atom y)) NIL)
      (T (cons (op (car x) (car y))
               (vopv op (cdr x) (cdr y)))))))

; V op
(define vop
  (lambda (op)
    (lambda (x y)
      (cond
        ((atom x) (vopx op y x))
        ((atom y) (vopx op x y))
        (T (vopv op x y))))))

; f/x
(define over
  (lambda (op x)
    (fold op (car x) (cdr x))))

; f\x
(define scan
  (lambda (op x)
    (reverse (fold (lambda (acc x)
                      (cons (op x (car acc)) acc))
                  (cons (car x) NIL)
                  (cdr x)))))

;; statistics
; average
(define Avg
  (lambda (x)
    (/ (fold + 0 x) (length x))))

; variance
(define Var
  (lambda (x)
    (let ((x2 (vopv * x x)))
      (- (Avg x2) (square (Avg x))))))

; std deviation
(define SD
  (lambda (x)
    (sqrt (Var x))))
