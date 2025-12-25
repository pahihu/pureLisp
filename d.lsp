;;; Symbolic derivation
;;;
; Helper: product of a list
(define product
  (lambda (xs)
    (cond
      ((atom xs) xs)
      ((atom (cdr xs)) (car xs))
      (T (cons '* xs)))))

; Derivative of an N-ary product: d(Prod xs) = x0' * Prod(tail) + x0 * d(Prod tail)
(define dprod
  (label dprod
    (lambda (xs var)
      (cond
        ((atom xs) 0)                         ; empty -> 0
        ((atom (cdr xs)) (d (car xs) var))    ; single factor -> its derivative
        (T `(+
              (* ,(d (car xs) var) ,(product (cdr xs)))
              (* ,(car xs) ,(dprod (cdr xs) var))))))))

; Derivative of a quotient u / v (and generalized u / ? tail)
(define dquot
  (lambda (u tail var)
    (let ((v (product tail)))
      `(/ (- (* ,(d u var) ,v)
             (* ,u ,(dprod tail var)))
          (* ,v ,v)))))

; Main derivative function
(define d
  (lambda (expr var)
    (cond
      ; Constant vs variable
      ((atom expr)
        (cond
          ((eq expr var) 1)   ; d(var)/d(var) = 1
          (T 0)))             ; d(const)/d(var) = 0

      ; Non-atomic expression: dispatch on operator
      (T (let ((op (car expr))
               (args (cdr expr)))
          (cond
            ; Sum: d(+ a b c ...) = + d(a) d(b) d(c) ...
            ((eq op (quote +))
              (cons (quote +)
                    (map (lambda (e) (d e var)) args)))

            ; Difference: d(- a b c ...) = - d(a) d(b) d(c) ...
            ((eq op (quote -))
              (cons (quote -)
                    (map (lambda (e) (d e var)) args)))

            ; Product: N-ary product rule
            ((eq op (quote *))
              (dprod args var))

            ; Quotient: treat denominator as product of tail
            ((eq op (quote /))
              (cond
                ((atom args) 0)                             ; (/ ) degenerate
                ((atom (cdr args)) (dquot (car args) NIL var))
                (T (dquot (car args) (cdr args) var))))

            ; Default: unknown operator › 0 (treat as constant form)
            (T 0)))))))


;; Simplify expression
; fold constants in a list with op
(define fold-constants
  (lambda (op xs)
    (cond
      ((atom xs) NIL)
      (T
        (let ((head (car xs))
              (tail (fold-constants op (cdr xs))))
          (cond
            ((atom head)
              (cond
                ((eq op (quote +))
                  (cons head tail))
                ((eq op (quote *))
                  (cons head tail))
                (T (cons head tail))))
            (T (cons head tail))))))))

; combine like terms in sums
(define combine-sum
  (lambda (xs)
    (cond
      ((atom xs) xs)
      (T
        (let ((x (car xs))
              (rest (combine-sum (cdr xs))))
          (cond
            ((atom rest) (cons x rest))
            ((equal x (car rest))
              (cons (cons '* (list 2 x)) (cdr rest)))
            (T (cons x rest))))))))

;; simplify expression
(define simplify
  (lambda (expr)
    (cond
      ; Atoms (constants or variables)
      ((atom expr) expr)
      (T (let ((op (car expr))
               (args (map simplify (cdr expr))))
          (cond
            ; + simplification
            ((eq op (quote +))
              (let ((filtered (remove 0 args)))
                (cond
                  ((atom filtered) 0)
                  ((atom (cdr filtered)) (car filtered))
                  (T (cons '+ filtered)))))

            ; * simplification
            ((eq op (quote *))
              (cond
                ((member 0 args) 0)
                (T (let ((filtered (remove 1 args)))
                    (cond
                      ((atom filtered) 1)
                      ((atom (cdr filtered)) (car filtered))
                      (T (cons '* filtered)))))))

            ; - simplification
            ((eq op (quote -))
              (cond
                ((atom args) 0)
                ((eq (car (cdr args)) 0) (car args))
                ((atom (cdr args)) (car args))
                (T (cons (quote -) args))))

            ; / simplification
            ((eq op (quote /))
              (cond
                ((eq (car (cdr args)) 1) (car args))
                (T (cons (quote /) args))))

            ; default: reconstruct
            (T (cons op args))))))))
