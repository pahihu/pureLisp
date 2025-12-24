;;; Symbolic derivation
;;;
; Helpers: sum and product of a list
(define sum
  (lambda (xs)
    (cons '+ xs)))

(define product
  (lambda (xs)
    (cond
      ((atom xs) xs)
      ((eq 1 (length xs)) (car xs))
      (T (cons '* xs)))))
    
(define list3
  (lambda (x y z)
    (cons x (list y z))))

; Derivative of an N-ary product: d(Prod xs) = x0' * Prod(tail) + x0 * d(Prod tail)
(define dprod
  (label dprod
    (lambda (xs var)
      (cond
        ((atom xs) 0)                         ; empty › 0
        ((atom (cdr xs)) (d (car xs) var))    ; single factor › its derivative
        (T (list3 '+ 
              (list3 '* (d (car xs) var) (product (cdr xs)))
              (list3 '* (car xs) (dprod (cdr xs) var))))))))

; Derivative of a quotient u / v (and generalized u / ? tail)
(define dquot
  (lambda (u tail var)
    (let ((v (product tail)))
      (/ (- (* (d u var) v)
            (* u (dprod tail var)))
         (* v v)))))

(define d0
  (lambda (op args var)
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
      (T 0))))

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
      (T (d0 (car expr) (cdr expr) var)))))

(define simplify-add
  (lambda (filtered)
    (cond
      ((atom filtered) 0)
      ((atom (cdr filtered)) (car filtered))
      (T (cons (quote +) (reverse filtered))))))

(define simplify-mul
  (lambda (filtered)
    (cond
      ((atom filtered) 1)
      ((atom (cdr filtered)) (car filtered))
      (T (cons (quote *) (reverse filtered))))))

(define simplify0
  (lambda (op args)
    (cond
      ; + simplification
      ((eq op (quote +))
        (simplify-add (fold (lambda (acc x)
                              (cond
                                ((eq x 0) acc)
                                ((and (atom x) (eq acc NIL)) (cons x NIL))
                                (T (cons x acc))))
                        NIL args)))
      ; * simplification
      ((eq op (quote *))
        (cond
          ((or (eq (car args) 0)
               (eq (car (cdr args)) 0)) 0)
          (T (simplify-mul (fold (lambda (acc x)
                                   (cond
                                     ((eq x 1) acc)
                                     (T (cons x acc))))
                             NIL args)))))
      ; - simplification
      ((eq op (quote -))
        (cond
          ((atom args) 0)
          ((atom (cdr args)) (car args))
          (T (cons (quote -) args))))

      ; / simplification
      ((eq op (quote /))
        (cond
          ((eq (car (cdr args)) 1) (car args))
          (T (cons (quote /) args))))

      ; default: reconstruct
      (T (cons op args)))))

;; simplify expression
(define simplify
  (lambda (expr)
    (cond
      ; Atoms (constants or variables)
      ((atom expr) expr)
      (T (simplify0 (car expr) (map simplify (cdr expr)))))))
