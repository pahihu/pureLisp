;;; Standard Library (in pure Lisp)

(define not (lambda (x) (cond (x NIL) (T T))))

(define and (lambda (x y) (cond (x y) (T NIL))))

(define or (lambda (x y) (cond (x T) (T y))))

(define list (lambda (a b) (cons a (cons b NIL))))

(define number? (lambda (x) (or (int? x) (float? x))))

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
      (T (cons (list (car x) (car y))
               (pair (cdr x) (cdr y)))))))

;; (assoc k L)
(define assoc
  (lambda (x y)
    (cond
      ((atom y) NIL)
      ((eq (car (car y)) x) (car (cdr (car y))))
      (T (assoc x (cdr y))))))

;; (member x L)
(define member
  (lambda (x y)
    (cond
      ((atom y) NIL)
      ((eq x (car y)) T)
      (T (member x (cdr y))))))

; (count x L)
(define count
  (lambda (x y)
    (let ((cnt (lambda (cnt x y n)
                (cond
                  ((atom y) n)
                  ((eq (car y) x) (cnt cnt x (cdr y) (+ n 1)))
                  (T (cnt cnt x (cdr y) n))))))
      (cnt cnt x y 0))))

;; (remove x L)
(define remove
  (lambda (x y)
    (cond
      ((atom y) NIL)
      ((eq x (car y)) (remove x (cdr y)))
      (T (cons (car y) (remove x (cdr y)))))))

(define equal
  (lambda (x y)
    (cond
      ((and (atom x) (atom y)) (eq x y))
      ((and (not (atom x)) (not (atom y)))
        (and (equal (car x) (car y))
             (equal (cdr x) (cdr y))))
      (T NIL))))

(defmacro elapsed
  (lambda (expr)
    (let ((t0 't0)
          (t1 't1)
          (res 'res))
      `(let ((,t0 (time-ms)))
         (let ((,res ,expr))
           (let ((,t1 (time-ms)))
             (list (- ,t1 ,t0) ,res)))))))

(defmacro times
  (lambda (n expr)
    (let ((loop 'loop)
          (i 'i)
          (s 's))
      `(let ((,loop
              (lambda (,loop ,i ,s)
                (cond
                  ((< ,i ,n) (,loop ,loop (+ ,i 1) ,expr))
                  (T ,s)))))
        (,loop ,loop 0 NIL)))))
