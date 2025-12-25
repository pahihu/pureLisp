; macros
(defmacro when
  (lambda (args)
    (list (quote cond) (list (car args) (car (cdr args))))))
          
(when T (quote ok))

; FAIL
(defmacro infix
  (lambda (args)
    (cons (car (cdr args)) (list (car args) (car (cdr (cdr args)))))))
          
((lambda (a b)
  (infix a + b)) 1 2)


; FAIL
(defmacro fn
  (lambda (args)
    (cons (quote lambda) (list (car args) (car (cdr args))))))
          
((fn (x) (cons x x)) (quote (z z)))
