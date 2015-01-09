#lang racket

(require racket/match)

;; Evaluation toggles between eval and apply.

; eval dispatches on the type of expression:
(define (eval exp env)
  (match exp
    [(? symbol?)          (env-lookup env exp)]
    [(? number?)          exp]
    [(? boolean?)         exp]
    [`(if ,ec ,et ,ef)    (if (eval ec env)
                              (eval et env)
                              (eval ef env))]
    [`(letrec ,binds ,eb) (eval-letrec binds eb env)]
    [`(let    ,binds ,eb) (eval-let    binds eb env)]
    [`(lambda ,vs ,e)    `(closure ,exp ,env)]
    [`(set! ,v ,e)        (env-set! env v e)]
    [`(begin ,e1 ,e2)     (begin (eval e1 env)
                                 (eval e2 env))]
    [`(,f . ,args)        (apply-proc
                           (eval f env) 
                           (map (eval-with env) args))]))

; a handy wrapper for Currying eval:
(define (eval-with env) 
  (lambda (exp) (eval exp env)))

; eval for letrec:
(define (eval-letrec bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (fs   (map (lambda _ #f) bindings))
         (env* (env-extend* env vars fs))
         (vals (map (eval-with env*) exps)))
    (env-set!* env* vars vals)
    (eval body env*)))

; eval for let:
(define (eval-let bindings body env)
  (let* ((vars (map car bindings))
         (exps (map cadr bindings))
         (vals (map (eval-with env) exps))
         (env* (env-extend* env vars vals)))
    (eval body env*)))
    
; applies a procedure to arguments:
(define (apply-proc f values) 
  (match f
    [`(closure (lambda ,vs ,body) ,env) 
     ; =>
     (eval body (env-extend* env vs values))]
    
    [`(primitive ,p)
     ; =>
     (apply p values)]))

;; Environments map variables to mutable cells 
;; containing values.

(define-struct cell ([value #:mutable]))

; empty environment:
(define (env-empty)  (hash))

; initial environment, with bindings for primitives:
(define (env-initial)
  (env-extend* 
   (env-empty)
   '(+  -  /  *  <=  void  display  newline)
   (map (lambda (s) (list 'primitive s))
   `(,+ ,- ,/ ,* ,<= ,void ,display ,newline))))

; looks up a value:
(define (env-lookup env var)
  (cell-value (hash-ref env var)))

; sets a value in an environment:
(define (env-set! env var value)
  (set-cell-value! (hash-ref env var) value))

; extends an environment with several bindings:
(define (env-extend* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (env-extend* (hash-set env v (make-cell val)) vars values)]
    
    [`(() ())
     ; =>
     env]))

; mutates an environment with several assignments:
(define (env-set!* env vars values)
  (match `(,vars ,values)
    [`((,v . ,vars) (,val . ,values))
     ; =>
     (begin
       (env-set! env v val)
       (env-set!* env vars values))]
    
    [`(() ())
     ; =>
     (void)]))

;; Evaluation tests.

; define new syntax to make tests look prettier:
(define-syntax 
  test-eval 
  (syntax-rules (====)
    [(_ program ==== value)
     (let ((result (eval (quote program) (env-initial))))
       (when (not (equal? program value))
         (error "test failed!")))]))

(test-eval
  ((lambda (x) (+ 3 4)) 20)
  ====
  7)

(test-eval
  (letrec ((f (lambda (n) 
                 (if (<= n 1)
                     1
                     (* n (f (- n 1)))))))
    (f 5))
  ====
  120)

(test-eval
  (let ((x 100))
    (begin
      (set! x 20)
      x))
  ====
  20)

(test-eval
  (let ((x 1000))
    (begin (let ((x 10))
             20)
           x))
  ====
  1000)

;; Programs are translated into a single letrec expression.

(define (define->binding define)
  (match define
    [`(define (,f . ,formals) ,body)
     ; =>
     `(,f (lambda ,formals ,body))]
    
    [`(define ,v ,value)
     ; =>
     `(,v ,value)]
    
    [else 
     ; =>
     `(,(gensym) ,define)]))

(define (transform-top-level defines)
  `(letrec ,(map define->binding defines)
     (void)))

(define (eval-program program)
  (eval (transform-top-level program) (env-initial)))

(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-all)))))

; read in a program, and evaluate:
(eval-program (read-all))
