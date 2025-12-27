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
                ((atom args) 0) ; (/ ) degenerate
                ((atom (cdr args)) (dquot (car args) NIL var))
                (T (dquot (car args) (cdr args) var))))

            ; d(expt u n, var) = (* n (expt u (- n 1)) (d u var))
            ((eq op (quote expt))
              (let ((u (car args))
                    (n (car (cdr args))))
                (cond
                  ((atom n) `(* ,n (expt ,u ,(- n 1)) ,(d u var)))
                  (T 0)))) ; only integer exponents supported

            ; d(log u, var) = (* (/ 1 u) (d u var))
            ((eq op (quote log))
              (cond
                ((atom args) 0)
                (T `(* (/ 1 ,@args) ,@(map (lambda (e) (d e var)) args)))))

            ; d(sin u, var) = (* (cos u) (d u var))
            ((eq op (quote sin))
              (cond
                ((atom args) 0)
                (T `(* (cos ,@args) ,@(map (lambda (e) (d e var)) args)))))

            ; d(cos u, var) = (* -1 (sin u) (d u var))
            ((eq op (quote cos))
              (cond
                ((atom args) 0)
                (T `(* -1 (sin ,@args) ,@(map (lambda (e) (d e var)) args)))))

            ; d(tan u, var) = (/ 1 (expt (cos u) 2)) (d u var))
            ((eq op (quote tan))
              (cond
                ((atom args) 0)
                (T `(* (/ 1 (expt (cos ,@args) 2)) ,@(map (lambda (e) (d e var)) args)))))

            ; Default: unknown operator -> 0 (treat as constant form)
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

; (collect-terms x rest 1 NIL)
; (collect-terms x '(x y x x y) 1 NIL)
; =>
; ['x', 4, ['y', 'y']]
(define collect-terms
  (lambda (x rest n collected)
    (cond
      ((atom rest) (cons x (list n collected)))
      ((eq x (car rest))
        (collect-terms x
                       (cdr rest)
                       (+ n 1)
                       collected))
      (T (collect-terms x
                        (cdr rest)
                        n
                        (cons (car rest) collected))))))


; (flatten-terms op terms NIL NIL)
; (flatten-terms '+ '((+ x y) (* x y) (+ x x)) NIL NIL)
; =>
; ['x', 'x', 'x', 'y', ['*', 'x', 'y']]
(define flatten-terms
  (lambda (op terms flat-terms other-terms)
    (cond
      ((atom terms) (append flat-terms other-terms))
      ((eq op (car (car terms)))
        (flatten-terms op
                       (cdr terms)
                       (append (cdr (car terms)) flat-terms)
                       other-terms))
      (T (flatten-terms op
                        (cdr terms)
                        flat-terms
                        (cons (car terms) other-terms))))))

; (+ ... (+ ...)) => (+ ... ...)
(define flatten
  (lambda (xs)
    (cond
      ((atom xs) xs)
      (T (cons (car xs) (flatten-terms (car xs) (cdr xs) NIL NIL))))))


; combiner
(define combiner.ga
  (lambda (op xs f)
    (let ((args (cdr xs)))
      (let ((x (combine-terms (car args)))
            (rest (map combine-terms (cdr args))))
        (cond
          ((atom rest) (cons op (cons x rest)))
          ; (op a a...)
          ((eq x (car rest))
            `(,op ,(f x) ,@(cdr rest)))
          ; (op a (op...) ...)
          ((and (not (atom (car rest))) (eq (car (car rest)) op))
            `(,op ,x ,@(cdr (car rest)) ,@(cdr rest)))
          ; (op (op...) ...)
          ((and (not (atom x)) (eq (car x) op))
            `(,op ,@(cdr x) ,@rest))
          (T (cons op (cons x rest))))))))

(define combiner
  (lambda (op xs f)
    (let ((args (cdr xs)))
      (let ((x (combine-terms (car args)))
            (rest (map combine-terms (cdr args))))
        (cond
          ((atom rest) (cons op (cons x rest)))
          ; (op a a...)
          ((> (count x rest) 0)
            (let ((ret (collect-terms x rest 1 NIL)))
              (let ((n (car (cdr ret)))
                    (collected (car (cdr (cdr ret)))))
                (cond
                  ((atom collected) (f x n))
                  (T `(,op ,(f x n) ,@collected))))))
          ; (op ... (op...) ...)
          (T (flatten (cons op (cons x rest)))))))))

(define combine-terms
  (lambda (xs)
    (cond
      ((atom xs) xs)
      ((eq (car xs) (quote *))
        (combiner '* xs (lambda (x n) `(expt ,x ,n))))
      ((eq (car xs) (quote +))
        (combiner '+ xs (lambda (x n) `(* ,x ,n))))
      (T (cons (car xs) (map combine-terms (cdr xs)))))))

(define int?
  (lambda (x)
    (and (number? x) (not (float? x)))))

(define expand-times
  (lambda (lst i n expr)
    (cond
      ((< i n) (expand-times (cons expr lst) (+ i 1) n expr))
      (T lst))))

(define expander
  (lambda (x y)
    (cond
      ((int? x) (expand-times '(+) 0 x y))
      ((int? y) (expand-times '(+) 0 y x))
      (T (cons y (list x '*))))))

(define expand-mul*
  (lambda (xs)
    (let ((op (car xs))
          (rest (cdr xs)))
      (cond
        ((and (eq op (quote *)) (eq 2 (length rest)))
          (reverse (expander (car rest) (car (cdr rest)))))
        (T xs)))))

; (* expr N) => (+ expr...)
(define expand-mul
  (lambda (xs)
    (cond
      ((atom xs) NIL)
      ((eq (car xs) (quote *)) (expand-mul* xs))
      (T (cons (car xs) (map expand-mul* (cdr xs)))))))


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

            ; expt simplification
            ((eq op (quote expt))
              (cond
                ((eq (car (cdr args)) 0) 1)
                ((eq (car (cdr args)) 1) (car args))
                (T (cons (quote expt) args))))

            ; default: reconstruct
            (T (cons op args))))))))

; apply f(x[n]) = x[n+1], until x[n+1] in prev. x[]
(define stable*
  (lambda (f X)
    (let ((x1 (f (car X))))
      (cond
        ((member x1 X) (car X))
        (T (stable* f (cons x1 X)))))))

(define stable
  (lambda (f x)
    (stable* f (cons x NIL))))

; return f(g(x))
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define full-simplify
  (lambda (x)
    (stable (compose combine-terms simplify) x)))
