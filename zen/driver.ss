(load "core.ss")
(load "parser.ss")
(load "bruijn.ss")
(load "zinc.ss")

(define (bruijn ast)
  (ast2lambda (empty-env) ast))

(define (step-compile ir)
  (compile ir (cons (IStop) '()) 0))
