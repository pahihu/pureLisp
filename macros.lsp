; macros
(defmacro when
  (lambda (cc body)
    (list (quote cond) (list cc body))))
          
(when T (quote ok))

; FAIL
(defmacro infix
  (lambda (x op y)
    (cons op (list x y))))
          
((lambda (a b)
  (infix a + b)) 1 2)


; FAIL
(defmacro fn
  (lambda (args body)
    (cons (quote lambda) (list args body))))
          
((fn (x) (cons x x)) (quote (z z)))


(defmacro prin
  (lambda (x)
    `(print ,@x)))
