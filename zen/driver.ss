(load "core.ss")
(load "parser.ss")
(load "bruijn.ss")
(load "zinc.ss")
(load "emit.ss")

(define (bruijn ast)
  (ast2lambda (empty-env) ast))

(define (step-compile ir)
  (compile ir (cons (IStop) '()) 0))

(define (step-emit bc)
  (let ((p
         (open-file-output-port "test.out"
                                (file-options no-fail)
                                (buffer-mode block))))
    (for-each (lambda (x) (emit-inst p x)) bc)
    (flush-output-port p)
    (close-output-port p)))
