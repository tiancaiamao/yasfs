#lang racket
(require r5rs)
(define (c-repr exp)
  (define symbol-map ;; map to create legal C names.
    '((#\- . #\_) (#\/ . #\S) (#\? . #\P) (#\> . #\G) (#\< . #\L) (#\= . #\E)
      (#\! . #\1) (#\+ . #\A) (#\* . #\C) (#\/ . #\D) (#\% . #\F)))
  (define (symbol-fix str pos)
    (if (= pos (string-length str)) '()
	(let ((a (assoc (string-ref str pos) symbol-map)))
	  (cons (if a (cdr a) (string-ref str pos))
		(symbol-fix str (+ 1 pos))))))
  (cond ((number? exp) (c "make_number(" (number->string exp) ")"))
        ((symbol? exp) (list->string (symbol-fix (symbol->string exp) 0)))
        ((char? exp) (c "make_character(\'"  exp "\')"))
        (else exp)))

(define (c-store value) (c "value = " value ";"))
;; generate locals, string constants and functions.
(define (c . strs) ;; generalized string-append
  (define (str-app str1 rest)
    (if (null? rest) (if (string? str1) str1 (c-repr str1))
	(string-append (if (string? str1) str1 (c-repr str1)) 
		       (str-app (car rest) (cdr rest)))))
  (str-app (car strs) (cdr strs)))

(define (append-code2 instrs)
    (cond ((null? instrs) instrs)
          ((null? (car instrs)) (append-code2 (cdr instrs)))
          ((pair? (car instrs))
           (append (car instrs) (append-code2 (cdr instrs))))
          (else (cons (car instrs) (append-code2 (cdr instrs))))))
(define (append-code . instrs) (append-code2 instrs))

(define *env* '())
(define *quote* '())
(define env-init-compiletime '())
(define env-global-compiletime '())
(define function '())
;;(define env-global-runtime (make-vector 200 '()))
(define scheme-unspecified (cons 'special 'unspecified))

(define (init-core-form)
  (define (add-syntax name)
    (let ((value `(,name predefined syntax . ,name)))
      (set! env-init-compiletime (cons value env-init-compiletime))))
  (add-syntax 'begin)
  (add-syntax 'if)
  (add-syntax 'set!)
  (add-syntax 'lambda)
  (add-syntax 'quote)
  (add-syntax 'define))

(define (init-primitive-form)
  (define (add-primitive name id arity)
    (let ((value `(,name predefined primitive ,id . ,arity)))
      (set! env-init-compiletime (cons value env-init-compiletime))))
  (add-primitive 'cons 0 2)
  (add-primitive '+ 1 2)
  (add-primitive 'car 2 1)
  (add-primitive 'cdr 3 1)
  (add-primitive 'null? 4 1))
(define (init)
  (init-core-form)
  (init-primitive-form))

(define (global-define name value)
  (let ((index (length env-global-compiletime)))
    (set! env-global-compiletime 
          (cons (cons name (cons 'global index)) env-global-compiletime))))
  ;;  (vector-set! env-global-runtime index value)))
;;(global-define 'test 35)

(define (CONSTANT value)
  (cond ((eq? value #t) (c-store "scheme_true"))
        ((eq? value #f) (c-store "scheme_false"))
        ((eq? value '()) (c-store "scheme_null"))
        ((eq? value scheme-unspecified) (c-store "scheme_unspecified"))
        ((string? value) (c "value = make_string(\"" value "\");"))
        ((number? value) (c-store value))
        (else
         (begin
           (set! *quote* (cons value *quote*))
           `(CONSTANT_QUOTE ,(- (length *quote*) 1))))))

(define (PREDEFINED v)
  (list 'PREDEFINED v))

(define (GLOBAL-REF i)
  (c "vm->value = GLOBAL_REF(" i ");"))

(define (ALTERNATIVE v1 v2 v3)
  (append-code v1 "if(vm->value != scheme_false){" v2 "}else{" v3))

(define (SEQUENCE v1 v2)
  (append-code v1 v2))
(define (GLOBAL-SET i v)
  (c "GLOBAL_SET(global," i "," v ");"))

(define (SHALLOW-ARGUMENT-REF j)
  (c "value = SHALLOW_ARGUMENT_REF(" (number->string j) ");"))
(define (SHALLOW-ARGUMENT-SET j value)
  (append value (list 'SHALLOW_ARGUMENT_SET j)))
(define (DEEP-ARGUMENT-SET i j value)
  (append value (list 'DEEP-ARGUMENT-SET i j)))
(define (DEEP-ARGUMENT-REF i j)
  (list 'DEEP-ARGUMENT-REF i j))
(define (CALL0 address)
  (lambda () (address)) )

(define (CALL1 address m1)
  (append
   m1
   (list 'ARG1)
   `(PRIMITIVE_CALL1 ,address)))

(define (CALL2 address m1 m2)
  (append
   m1
   (list 'ARG1)
   m2
   (list 'ARG2)
   `(PRIMITIVE_CALL2 ,address)))

(define (CALL3 address m1 m2 m3)
  (append
   m1
   (list 'ARG1)
   m2
   (list 'ARG2)
   m3
   (list 'ARG3)
   `(PRIMITIVE_CALL3 ,address)))
  
;;函数调用规则：由调用者准备好参数。由被调函数切换环境绑定。由调用者恢复环境
(define (CLOSURE code arity size)
  (let ((fun-ptr (append-code "void funcXXX(struct vm *vm){" 
               (c "ENTER_CLOSURE(" (number->string size) ",\"funcXXX\");")
               code
               "EXIT_CLOSURE();" "}")))
    (begin
      (cons fun-ptr function)
      (c-store "make_closure(funcXXX,vm->env)")))  )
  
(define (invoke closure arg)
  (let ((function (car closure))
        (frame (cdr closure)))
    (function arg)))
(define (TAIL-CALL op arg)
  (append op
          (list 'VAL_FUN)
          arg
          (list 'ENV_STACK)
          (list 'TAIL_INVOKE)
          (list 'STACK_ENV)))

(define (CALL op arg)
  (append op
          (list 'VAL_FUN)
          arg
          (list 'ENV_STACK)
          (list 'INVOKE)
          (list 'STACK_ENV)))

(define (STORE-ARGUMENT m m* pos)
  (append m
          (list 'STACK_PUSH)
          m*
          (list 'SET_FRAME_ARGUMENT pos)))

(define (ALLOCATE-FRAME size)
  (list 'ALLOCATE_FRAME size))

(define (local-variable? env i name)
  (define (helper names j)
    (cond ((null? names) #f)
          (else
           (if (eqv? (car names) name)
               `(local ,i . ,j)
               (helper (cdr names) (+ j 1))))))
  (cond ((null? env) #f)
        (else
         (let ((names (car env)))
          (or (helper names 0)
              (local-variable? (cdr env) (+ i 1) name))))))

(define (global-variable? list name)
  (let ((find (assv name list)))
    (and find (cdr find))))

(define (compute-kind env name)
  (or (local-variable? env 0 name)
      (global-variable? env-global-compiletime name)
      (global-variable? env-init-compiletime name)))

(define (compile-wrong msg)
  (display msg))
(define (runtime-wrong msg)
  (display msg))

(define (compile-variable form env tail?)
  (let ((kind (compute-kind env form)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)))
             (if (= i 0)
                 (SHALLOW-ARGUMENT-REF j)
                 (DEEP-ARGUMENT-REF i j))))
          ((global)
           (let ((i (cdr kind)))
             (GLOBAL-REF i)))
          ((predefined)
           (let ((v (cdr kind)))
             (PREDEFINED v))))
        (compile-wrong "No such variable"))))

(define (compile-quote v)
  (CONSTANT v))
(define (compile-if etest etrue efalse env tail?)
  (let ((v1 (compile etest env #f))
        (v2 (compile etrue env tail?))
        (v3 (compile efalse env tail?)))
    (ALTERNATIVE v1 v2 v3)))
(define (compile-begin form env tail?)
  (define (inner-defines lst)
    (cond ((null? lst) '())
          (else
           (let ((obj (car lst)))
             (if (and obj (pair? obj) (eqv? (car obj) 'define))
                 (cons (if (pair? (cadr obj))
                           (caadr obj)
                           (cadr lst))
                       (inner-defines (cdr lst)))
                 (inner-defines (cdr lst)))))))
  (define (compile-it form env tail?)
    (if (pair? form)
        (if (pair? (cdr form))
            (let ((v1 (compile (car form) env #f))
                  (v2 (compile-it (cdr form) env tail?)))
              (SEQUENCE v1 v2))
            (compile (car form) env tail?))
        (CONSTANT scheme-unspecified)))
  (if (null? env)
      (compile-it form env tail?)
      (let ((names (inner-defines form)))
        (if (null? names)
            (compile-it form env tail?)
            (begin (set-car! env (append (car env) names))
                   (compile-it form env tail?))))))
        

(define (compile-define-global name form env tail?)
  (let  ((locate (global-variable? env-global-compiletime name)))
    (if locate
        (GLOBAL-SET (cdr locate) (compile form env #f))
        (begin 
          (global-define name scheme-unspecified)
          (compile-define-global name form env tail?)))))

#|(define (compile-define-inner name form env tail?)
  (define (helper name lst idx)
    (cond ((null? lst) #f)
          ((eqv? (car lst) name) idx)
          (else
           (helper name (cdr lst) (+ idx 1)))))
  (let ((locate (helper name (car env) 0))
        (value (compile form env #f)))
    (if locate
        (SHALLOW-ARGUMENT-SET locate value)
        (begin 
          (set-car! env (cons name (car env)))
          (compile-define-inner name form env tail?)))))     
|#  
(define (compile-define name form env tail?)
  (if (pair? name)
      (compile-define (car name) `(lambda ,(cdr name) ,form) env tail?)    
      (if (null? env)
          (compile-define-global name form env tail?)
          (compile-set name form env tail?))))

(define (compile-set name form env tail?)
  (let ((value (compile form env #f))
        (kind (compute-kind env name)))
    (if kind
        (case (car kind)
          ((local) 
           (let ((i (cadr kind))
                 (j (cddr kind)))
             (if (= i 0)
                 (SHALLOW-ARGUMENT-SET j value)
                 (DEEP-ARGUMENT-SET i j value))))
          ((global) 
           (GLOBAL-SET (cdr kind) value))
          ((predefined) 
           (compile-wrong "Immutable predefined variable")))
        (compile-wrong "No such variable"))))
(define (extend-env env names)
  (cons names env))

(define (compile-lambda names body env tail?)
  (let* ((new-env (extend-env env names))
         (v (compile-begin body new-env #t))
	 (arity (length names))
	 (size (length (car new-env)))) ;;note here!size is not arity because of inner define
    (CLOSURE v arity size)))

(define (compile-argument-recurse e e* env size pos tail?)
  (let ((v (compile e env #f))
        (v* (compile-argument e* (- size 1) (+ pos 1) env tail?)))
    (STORE-ARGUMENT v pos v*)))
(define (compile-argument arg pos env tail?)
  (if (pair? arg)
      (let ((v (compile (car arg) env #f))
            (v* (compile-argument (cdr arg) (+ pos 1) env tail?)))
        (STORE-ARGUMENT v v* pos))
      (ALLOCATE-FRAME pos)))

(define (compile-regular-application first other env tail?)
  (let ((op (compile first env tail?))
        (arg (compile-argument other 0 env tail?)))
    (append-code arg op
       "vm->func = vm->value;"
       "vm->value = make_vector(7);"
       "vector_set(vm->value,4,POP());"
       "vector_set(vm->value,5,POP());"
       "CLOSURE_CALL()")))

(define (compile-primitive-call info name other env tail?)
  (let ((address (car info))
        (arity (cdr info)))
    (if (= arity (length other))
        (case arity
          ((0) (c "vm->value = " name "();"))
          ((1) 
           (let ((m1 (compile (car other) env #f)))
             (append-code m1
                          "PUSH(vm->value);"
                          (c "vm->value = " name "(POP());"))))
          ((2)
           (let ((m1 (compile (car other) env #f))
                 (m2 (compile (cadr other) env #f)))
             (append-code m1 
                          "PUSH(vm->value);"
                          m2
                          "PUSH(vm->value);"
                          (c "vm->value = " name "(POP(),POP())"))))
          ((3)
           (let ((m1 (compile (car other) env #f))
                 (m2 (compile (cadr other) env #f))
                 (m3 (compile (caddr other) env #f)))
             (append-code m1
                          "PUSH(vm->value);"
                          m2
                          "PUSH(vm->value);"
                          m3
                          "PUSH(vm->value);"
                          (c "vm->value = " name "(POP(),POP(),POP())"))))
          (else 
           (compile-wrong "support atmost 3 arity privitive now!")))
        (compile-wrong "Incorrect arity for primitive"))))

(define (compile-special-form type other env tail?)
  (case type
    ((quote) (compile-quote (car other)))
    ((if) (compile-if (car other) (cadr other) (caddr other) env tail?))
    ((begin) (compile-begin other env tail?))
    ((set!) (compile-set (car other) (cadr other) env tail?))
    ((lambda) (compile-lambda (car other) (cdr other) env tail?))
    ((define) (compile-define (car other) (cadr other) env tail?))))

(define (predefined-kind? kind)
  (and (pair? kind) (eq? (car kind) 'predefined)))
(define (compile-application first other env tail?)
  (if (symbol? first)
      (let ((kind (compute-kind env first)))
        (if (predefined-kind? kind)            
            (let ((desc (cdr kind)))
              (case (car desc)
                ((syntax) 
                 (compile-special-form (cdr desc) other env tail?))
                ((primitive) 
                 (compile-primitive-call (cdr desc) first other env tail?))
                ((macro) '())))
            (compile-regular-application first other env tail?)))
      ;;	 ((and (pair? op) (eq? (car op) 'lambda))
      ;;	  (compile-closed-application op arg env tail?))
      (compile-regular-application first other env tail?)))

(define (compile form env tail?)
  (if (pair? form)
      (compile-application (car form) (cdr form) env tail?)
      (if (symbol? form)
          (compile-variable form env tail?)
          (CONSTANT form))))

(define (read-file filename)
    (call-with-input-file filename
      (lambda (in)
        (let gather ((e (read in))
                     (content '()) )
          (if (eof-object? e)
              (reverse content)
              (gather (read in) (cons e content)) ) ) ) ) )


(define (write-result-file ofilename comments global-size constants code)
  (call-with-output-file ofilename
    (lambda (out)
      (for-each (lambda (comment) (display comment out)) comments)
      (newline out) (newline out)
      (display ";;constant quote" out) (newline out)
      (write constants out) (newline out) (newline out)
      (display ";;global size" out) (newline out)
      (write global-size out) (newline out) (newline out)
      (display ";;code" out) (newline out)
      (write code out) )))
    
(define (compile-file filename)
  (set! env-global-compiletime '())
  (set! *quote* '())
  (let* ((source `(begin . ,(read-file filename)))
         (asm-code (compile source '() #t))
         (bytecode (assemble->bytecode asm-code))
         (constants (apply vector *quote*))
         (global-size (length env-global-compiletime))
         (ofilename (string-append filename ".so")))
    (write-result-file ofilename
                       (list ";;Bytecode for " filename)
                       global-size constants bytecode)))   

(define (instruct->bytecode asym)
  (let ((find (assv asym instruct-table)))
    (if find
        (cadr find)
        (compile-wrong (string-append "bad insruct:" (symbol->string asym)) ))))

(define (assemble->bytecode lst)
  (list->vector (map (lambda (obj) (if (symbol? obj)
                         (instruct->bytecode obj)
                         obj))
       lst)))
      
;;a instruct is construct by '(name bytecode arity)
(define instruct-table
  '((CREATE_CLOSURE 0 2)
    (GOTO 1 1)
    (CHANGE_ENV 2 0)
    (SHALLOW_ARGUMENT_REF 3 1)
    (ARG1 4 0)
    (ARG2 5 0)
    (PRIMITIVE_CALL2 6 1)
    (RETURN 7 0)
    (GLOBAL_SET 8 1)
    (GLOBAL_REF 9 1)
    (VAL_FUN 10 0)
    (CONSTANT_NUM 11 1)
    (STACK_PUSH 12 0)
    (ALLOCATE_FRAME 13 1)
    (SET_FRAME_ARGUMENT 14 1)
    (ENV_STACK 15 0)
    (TAIL_INVOKE 16 0)
    (STACK_ENV 17 0)
    (INVOKE 18 0)
    (CONSTANT_TRUE 19 0)
    (CONSTANT_FALSE 20 0)
    (CONSTANT_NULL 21 0)
    (CONSTANT_UNSPECIFIED 22 0)
    (CHECKED_SHALLOW_ARGUMENT_REF 23 1)
    (SHALLOW_ARGUMENT_SET 24 1)
    (FINISH 24 0)))
(define (export-instruct-table)
  (for-each (lambda (obj) (display (car obj)) (display #\,) (newline)) instruct-table))