#lang eopl

;; Exercise 1.12 [★] Eliminate the one call to subst-in-s-exp in subst by replacing it by its definition and
;; simplifying the resulting procedure. The result will be a version of subst that does not need subst-in-s-exp. This
;; technique is called inlining, and is used by optimizing compilers.

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (let ([sexp (car slist)])
                (if (symbol? sexp)
                    (if (eqv? sexp old) new sexp)
                    (subst new old sexp)))
              (subst new old (cdr slist))))))

(provide subst)
