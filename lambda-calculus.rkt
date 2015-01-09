#lang racket
(racket racket/match)

(define (eval exp env)
  (match exp
    [`(,f ,e) (apply (eval f env) (eval e ent))]
    [`(λ ,v . ,e) `(closure ,exp ,env)]
    [(? symbol?) (cadr (assq exp env))]))

(define (apply f x)
  (match f
    [`(closure (λ ,v . ,body) ,env)
     (eval body (cons `(,v ,x) env))]))

(display (eval (read) '())) (newline)