
(eval-when (compile load eval)
  (optimize-level 2)
  (case-sensitive #t)
)

(load "match.ss")
(load "helpers.ss")
(load "fmts.pretty")     ; inform pretty-print about new forms
(load "driver.ss")

(load "a15.ss")
(load "a15-wrapper.ss")   ; defines syntactic forms and procedures
                         ; needed to output of each pass


#!eof

(compiler-passes '(
                   parse-scheme
                   convert-complex-datum
                   uncover-assigned
                   #|
                   purify-letrec
                   convert-assignments
                                        ;optimize-direct-call ;;; optimization
                   remove-anonymous-lambda
                   sanitize-binding-forms
                   uncover-free
                   convert-closures
                                        ;optimize-known-call ;;; optimization
                                        ;optimize-self-reference ;;; optimization
                   introduce-procedure-primitives
                   lift-letrec
                   normalize-context
                   specify-representation
                   uncover-locals
                   remove-let
                   verify-uil
                   remove-complex-opera*
                   flatten-set!
                   impose-calling-conventions				
                   uncover-frame-conflict
                   pre-assign-frame
                   assign-new-frame
                   (iterate
                    finalize-frame-locations
                    select-instructions
                    uncover-register-conflict
                    assign-registers
                    (break when everybody-home?)
                    assign-frame)
                   discard-call-live
                   finalize-locations
                   expose-frame-var
                   expose-basic-blocks
                   flatten-program
                   generate-x86-64
                   |#
                   ))

;; (load "tests15.ss")

;; (tracer '(parse-scheme))

#|
(test-one '(let ([f (lambda (y)
                    (lambda (p)
                      (cons y (p y))))])
              (letrec ([g (lambda (n)
                (if (= n 0)
                    '()
                    ((f (- n 1)) g)))])
    (g 6))))
|#
