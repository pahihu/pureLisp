;;; Standard Library (in pure Lisp)

(define not (lambda (x) (cond (x NIL) (T T))))

(define and (lambda (x y) (cond (x y) (T NIL))))

(define or (lambda (x y) (cond (x T) (T y))))

(define list (lambda (a b) (cons a (cons b NIL))))

(define null (lambda (x) (eq x NIL)))

;; (append L1 L2)
(define append
  (label append
    (lambda (x y)
      (cond
        ((atom x) y)
        (T (cons (car x) (append (cdr x) y)))))))

;; (map fn L)
(define map
  (label map
    (lambda (f xs)
      (cond
        ((atom xs) NIL)
        (T (cons (f (car xs)) (map f (cdr xs))))))))

;; (length L)
(define length
  (label length
    (lambda (xs)
      (cond
        ((atom xs) 0)
        (T (+ 1 (length (cdr xs))))))))

;; (reverse L)
(define reverse
  (label reverse
    (lambda (xs)
      (cond
        ((atom xs) NIL)
        (T (append (reverse (cdr xs)) (cons (car xs) NIL)))))))

;; (fold fn i L)
(define fold
  (label fold
    (lambda (f init xs)
      (cond
        ((atom xs) init)
        (T (fold f (f init (car xs)) (cdr xs)))))))

;; (pair K V) => ((k1 v1)...)
(define pair
  (lambda (x y)
    (cond
      ((and (atom x) (atom y)) NIL)
      ((and (not (atom x)) (not (atom y)))
        (cons (list (car x) (car y))
              (pair (cdr x) (cdr y)))))))

;; (assoc k L)
(define assoc
  (lambda (x y)
    (cond
      ((atom y) NIL)
      ((eq (car (car y)) x) (car (cdr (car y))))
      (T (assoc x (cdr y))))))
