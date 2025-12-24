;; Define expressions
(define x x)
(define y y)

; d(x, x) = 1
(d x x)                          ; => 1

; d(42, x) = 0
(d 42 x)                         ; => 0

; d(+ x 3 y, x) = + 1 0 0
(d '(+ x 3 y) x)                  ; => (+ 1 0 0)

; d(* x x x, x) = + (* 1 (* x x)) (* x (* 1 x)) (* x (* x 1))
(d '(* x x x) x)                  ; => (+ (* 1 (* x x)) (* x (* 1 x)) (* x (* x 1)))

; d(/ (* x x) y, x) = (/ (- (* (+ (* 1 x) (* x 1)) y)
;                           (* (* x x) (* 0 y)))
;                        (* y y))
(d '(/ (* x x) y) x)

; Derivative before simplification
(d (* x x x) x)
; => (+ (* 1 (* x x)) (* x (* 1 x)) (* x (* x 1)))

; After simplification
(simplify (d (* x x x) x))
; => (* 3 (* x x))

; Another example
(simplify '(+ 0 x 0))
; => x

(simplify '(* 1 x 1))
; => x

(simplify '(/ x 1))
; => x
