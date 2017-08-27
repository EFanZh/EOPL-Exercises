#lang eopl

;; Exercise 1.27 [🟉🟉] (flatten slist) returns a list of the symbols contained in slist in the order in which they occur
;; when slist is printed. Intuitively, flatten removes all the inner parentheses from its argument.
;;
;;     > (flatten '(a b c))
;;     (a b c)
;;     > (flatten '((a) () (b ()) () (c)))
;;     (a b c)
;;     > (flatten '((a b) c (((d)) e)))
;;     (a b c d e)
;;     > (flatten '(a b (() (c))))
;;     (a b c)

(define flatten-element
  (lambda (tail element)
    (if (list? element)
        (flatten-helper tail element)
        (cons element tail))))

(define flatten-helper
  (lambda (tail slist)
    (if (null? slist)
        tail
        (flatten-element (flatten-helper tail (cdr slist))
                         (car slist)))))

(define flatten
  (lambda (slist)
    (flatten-helper '() slist)))

(provide flatten)
