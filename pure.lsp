;
(list (quote a) (quote b))

(append (quote (a b)) (quote (c d)))

(reverse (quote (a b c)))

(map (lambda (x) (* x x)) (quote (1 2 3)))

(fold + 0 '(1 2 3 4 5))

(fold * 1 '(1 2 3 4 5))


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
