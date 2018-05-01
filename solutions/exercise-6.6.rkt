#lang racket

(define-syntax define-function
  (syntax-rules ()
    [(_ (f args ...)) (define (f args ...)
                        (list (quote f) args ...))]))

(define-syntax define-function/k
  (syntax-rules ()
    [(_ (f args ... cont)) (define (f args ... cont)
                             (cont (list (quote f) args ...)))]))

(define-function (+ x y))

(define-function/k (f x cont))
(define-function/k (g x cont))
(define-function/k (h x cont))
(define-function/k (j x cont))

(define solution1
  (lambda (x y cont)
    (g x
       (lambda (val1)
         (f val1
            (lambda (val2)
              (j y
                 (lambda (val3)
                   (h val3
                      (lambda (val4)
                        (cont (+ val2 val4))))))))))))

(define solution2
  (lambda (x y cont)
    (g x
       (lambda (val1)
         (j y
            (lambda (val2)
              (f val1
                 (lambda (val3)
                   (h val2
                      (lambda (val4)
                        (cont (+ val3 val4))))))))))))

(define solution3
  (lambda (x y cont)
    (g x
       (lambda (val1)
         (j y
            (lambda (val2)
              (h val2
                 (lambda (val3)
                   (f val1
                      (lambda (val4)
                        (cont (+ val4 val3))))))))))))

(define solution4
  (lambda (x y cont)
    (j y
       (lambda (val1)
         (g x
            (lambda (val2)
              (f val2
                 (lambda (val3)
                   (h val1
                      (lambda (val4)
                        (cont (+ val3 val4))))))))))))

(define solution5
  (lambda (x y cont)
    (j y
       (lambda (val1)
         (g x
            (lambda (val2)
              (h val1
                 (lambda (val3)
                   (f val2
                      (lambda (val4)
                        (cont (+ val4 val3))))))))))))

(define solution6
  (lambda (x y cont)
    (j y
       (lambda (val1)
         (h val1
            (lambda (val2)
              (g x
                 (lambda (val3)
                   (f val3
                      (lambda (val4)
                        (cont (+ val4 val2))))))))))))

(provide solution1 solution2 solution3 solution4 solution5 solution6)
