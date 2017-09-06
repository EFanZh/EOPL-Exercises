#lang eopl

;; Exercise 1.7 [★★] The error message from nth-element is uninformative. Rewrite nth-element so that it produces a
;; more informative error message, such as “(a b c) does not have 8 elements.”

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements.~%" lst (+ n 1))))

(define nth-element-helper
  (lambda (lst n current-list i)
    (if (null? current-list)
        (report-list-too-short lst n)
        (if (zero? i)
            (car current-list)
            (nth-element-helper lst n (cdr current-list) (- i 1))))))

(define nth-element
  (lambda (lst n)
    (nth-element-helper lst n lst n)))

(provide nth-element)
