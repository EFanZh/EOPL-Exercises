#lang eopl

;; Exercise 1.18 [🟉] (swapper s1 s2 slist) returns a list the same as slist, but with all occurrences of s1 replaced by
;; s2 and all occurrences of s2 replaced by s1.
;;
;;     > (swapper 'a 'd '(a b c d))
;;     (d b c a)
;;     > (swapper 'a 'd '(a d () c d))
;;     (d a () c a)
;;     > (swapper 'x 'y '((x) y (z (x))))
;;     ((y) x (z (y)))

(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (sexp)
           (if (symbol? sexp)
               (if (eqv? sexp s1)
                   s2
                   (if (eqv? sexp s2)
                       s1
                       sexp))
               (swapper s1 s2 sexp)))
         slist)))

(provide swapper)
