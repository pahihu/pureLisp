;
(list (quote a) (quote b))

(append (quote (a b)) (quote (c d)))

(reverse (quote (a b c)))

(map (lambda (x) (cons x x)) (quote (a b c)))


; macros
(defmacro when
  (lambda (args)
    (list (quote cond)
          (list (car args) (car (cdr args))))))
          
(when T (quote ok))

(defmacro infix
  (lambda (args)
    (list (car (cdr args))
          (car args)
          (car (cdr (cdr args))))))
          
(infix a + b)

(defmacro fn
  (lambda (args)
    (list (quote lambda)
          (car args)
          (car (cdr args)))))
          
((fn (x) (cons x x)) (quote z))


; arithmetic
(+ 1 2 3)

(- 10 3 2)

(* 2 3 4)

(/ 20 2 2)

(* 7 8)

(define square (lambda (x) (* x x)))

(square 12)

(map (lambda (x) (* x x)) (quote (1 2 3 4)))


; comparison
(define x 3)
(cond ((< x 10) (quote small)) (T (quote big)))


; quasiquote
(define x 10)
(define xs (list 1 2))
`(a ,x ,@xs b)
