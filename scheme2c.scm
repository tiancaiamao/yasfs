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

(define (c-store value) (c "vm->value = " value ";"))
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
(define macor-list '())
(define function '())
;;(define env-global-runtime (make-vector 200 '()))
(define scheme-unspecified (cons 'special 'unspecified))
(define standard-env (cons '() '()))

(define (init-core-form)
  (define (add-syntax name)
    (let ((value `(,name predefined syntax . ,name)))
      (set! env-init-compiletime (cons value env-init-compiletime))))
  (add-syntax 'begin)
  (add-syntax 'if)
  (add-syntax 'set!)
  (add-syntax 'lambda)
  (add-syntax 'quote)
  (add-syntax 'define)
  (add-syntax 'def-macro))

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
  (append-code value (c "vm->value = SHALLOW_ARGUMENT_SET(" (number->string j) ");")))
(define (CHECKED-SHALLOW-ARGUMENT-REF j)
  (append-code (c "vm->value = SHALLOW_ARGUMENT_REF(" (number->string j) ");")
               "if(value == scheme_unspecified)" "goto exit;"))
(define (DEEP-ARGUMENT-SET i j value)
  (append-code value 
               (c "value = DEEP_ARGUMENT_SET(" (number->string i) "," (number->string j) ");")))
(define (DEEP-ARGUMENT-REF i j)
  (c "value = DEEP_ARGUMENT_REF(" (number->string i) "," (number->string j) ");"))
(define (CHECKED-DEEP-ARGUMENT-REF i j)
  (append-code 
   (c "value = DEEP_ARGUMENT_REF(" (number->string i) "," (number->string j) ");")
   "if(value == scheme_unspecified)" "goto exit;"))
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
      (set! function (cons fun-ptr function))
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

(define (STORE-ARGUMENT m m* pos func)
  (append-code m
               "PUSH(vm->value);"
               m*
               (c "vector_set(vm->value," (number->string pos) ",POP())")
               ))
(define (ALLOCATE-FRAME size func)
  (append-code func
               "vm->func = vm->value;"
               (c "vm->value = make_vector(" (number->string size) ");")
               ))

(define (compute-kind env name)
  (or (env-variable? env 0 name)
      (predefined-variable? env-init-compiletime name)
      (macro? name)))
(define (macro? name)
  #f)
(define (env-variable? env i name)
  (define (helper names j)
    (cond ((null? names) #f)
          (else
           (cond 
             ((eqv? (car names) name)
              `(local ,i . ,j))
             ((and (pair? (car names)) 
                   (eqv? (caar names) name))
              `(inner-define ,i ,j . ,(cdar names)))
             (else
              (helper (cdr names) (+ j 1)))))))
  (cond ((null? env) #f)
        (else
         (let ((names (car env)))
           (or (helper names 0)
               (env-variable? (cdr env) (+ i 1) name))))))
(define (predefined-variable? list name)
  (let ((find (assv name list)))
    (and find (cdr find))))
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
          ((inner-define)
           (let ((i (cadr kind))
                 (j (caddr kind))
                 (inited (cdddr kind)))
             (cond 
               ((and inited (= i 0))
                (SHALLOW-ARGUMENT-REF j))
               ((and inited (not (= i 0)))
                (DEEP-ARGUMENT-REF i j))
               ((= i 0)
                (CHECKED-SHALLOW-ARGUMENT-REF j))
               (else 
                (CHECKED-DEEP-ARGUMENT-REF i j)))))
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
                           (cadr obj))
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
  (let ((names (inner-defines form)))
    (if (null? names)
        (compile-it form env tail?)
        (let* ((inner-define-names (map (lambda (obj) (cons obj #f)) names))
               (new-env-names (append (car env) inner-define-names))
               (new-env (cons new-env-names (cdr env))))
          (compile-it form new-env tail?)))))
        

#|(define (compile-define-global name form env tail?)
  (let  ((locate (global-variable? env-global-compiletime name)))
    (if locate
        (GLOBAL-SET (cdr locate) (compile form env #f))
        (begin 
          (global-define name scheme-unspecified)
          (compile-define-global name form env tail?)))))|#

(define (compile-define name form env tail?)
  (define (helper name lst idx)
    (if (null? lst)
        #f
        (cond 
          ((eqv? (car lst) name) idx)
          ((and (pair? (car lst)) (eqv? (caar lst) name))
           (begin (set-cdr! (car lst) #t)
                  idx))
          (else
           (helper name (cdr lst) (+ idx 1))))))
  (let ((locate (helper name (car env) 0))
        (value (compile form env #f)))
    (if locate
        (SHALLOW-ARGUMENT-SET locate value)
        (begin 
          (set-car! env (cons name (car env)))
          (compile-define name form env tail?)))))

#|(define (compile-define name form env tail?)
  (if (pair? name)
      (compile-define (car name) `(lambda ,(cdr name) ,form) env tail?)    
      (if (null? env)
          (compile-define-global name form env tail?)
          (compile-set name form env tail?))))|#

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
          ((inner-define)
           (let ((i (cadr kind))
                 (j (caddr kind))
                 (inited (cdddr kind)))
             (if inited
                 (if (= i 0)
                     (SHALLOW-ARGUMENT-SET j value)
                     (DEEP-ARGUMENT-SET i j value))
                 (compile-wrong "can't set undefined variable"))))
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

(define (compile-let other env tail?)
  (let ((names (map (lambda (pair) (car pair)) (car other)))
        (values (map (lambda (pair) (cadr pair)) (car other)))
        (body (cdr other)))
    (compile `((lambda ,names ,@body) ,@values) env tail?)))

(define (compile-argument arg pos env tail? func)
  (if (pair? arg)
      (let ((v (compile (car arg) env #f))
            (v* (compile-argument (cdr arg) (+ pos 1) env tail? func)))
        (STORE-ARGUMENT v v* pos func))
      (ALLOCATE-FRAME pos func)))

(define (STORE-FUNC v v*)
  (append-code v
               "vm->func = vm->value;"
               v*))
(define (CLOSURE-CALL v)
  (append-code v
               "CLOSURE_CALL()"))
(define (compile-regular-application first other env tail?)
  (let ((op (compile first env #f)))
    (CLOSURE-CALL (compile-argument other 0 env tail? op))))
    

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
    ((define) (compile-define (car other) (cadr other) env tail?))
    ((let) (compile-let other env tail?))))

(define (predefined-kind? kind)
  (and (pair? kind) (eq? (car kind) 'predefined)))
  
(define (compile-application first other env tail?)
  (if (symbol? first)
      (let ((kind (compute-kind env first)))
        (cond 
          ((predefined-kind? kind)            
           (let ((desc (cdr kind)))
             (case (car desc)
               ((syntax) 
                (compile-special-form (cdr desc) other env tail?))
               ((primitive) 
                (compile-primitive-call (cdr desc) first other env tail?)))))
          ((macro? kind)
           #f)
          (else
           (compile-regular-application first other env tail?))))
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


(define (write-result-file ofilename closures code)
  (call-with-output-file ofilename
    (lambda (out)
      (for-each (lambda (afun) 
                  (for-each (lambda (str) (display str out) (newline out)) afun))
                closures)
      (newline out) (newline out)
      (display "//code" out) (newline out)
      (for-each (lambda (str) (display str out) (newline out)) code))))
    
(define (compile-file filename)
  (let* ((source `((lambda () ,(read-file filename))))
         (code (compile source standard-env #t))
         (ofilename (string-append filename ".c")))
    (write-result-file ofilename function code)))

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
(init)