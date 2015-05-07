(define (prim? x) (memq x '(+ - * / = env-get env-make)))

(define split 
  (lambda (lst c)
    (cond
     ((null? lst) "")
     ((null? (cdr lst)) (car lst))
     (else 
      (string-append (car lst) c (split (cdr lst) c))))))

(define generate-lambda
  (lambda (bind body collect)
    (let* ((func-name (symbol->string (gensym 'lambda__tmp)))
	   (declear (string-append "void " func-name "(" 
				   (split (map (lambda (x) (string-append "Value " (symbol->string x))) bind) ", ") ")"))
	   (def (string-append declear " {\n" (generate body) "\n}\n")))
      (collect func-name declear def))))

(define global-funcs '())
(define global-vars '())

(define gensym
  (let ((v 0))
    (lambda (x)
      (set! v (+ v 1))
      (string->symbol
       (string-append (symbol->string x) (number->string v))))))

(define generate
  (lambda (exp)
    (match exp
	   [(? boolean?) (if (eq? exp #t) "ValueTrue" "ValueFalse")]
	   [(? integer?) 
	    (string-append "MakeInt(" (number->string exp) ")")]
	   [(? symbol?)
	    (if (prim? exp)
		(case exp
		  ['+ "__add"]
		  ['- "__sub"]
		  ['* "__product"]
		  ['env-get "VectorRef"]
		  ['env-make "MakeEnv"]
		  [else (symbol->string exp)])
		(symbol->string exp))]
	   [`(if ,test ,then ,else)
	    (string-append "if (" (generate test) " == ValueTrue) {\n"
			   (generate then) "\n"
			   "} else {\n"
			   (generate else) "\n}")]
	   [('begin exps ...)
	    (split (map generate exps) "\n")]
	   [`(set! ,var ,val)
	    (string-append (generate var) " = " (generate val) "\n")]
	   [`(define ,var ,val)
	    (set! global-vars
		  (cons (string-append "Value " (symbol->string var) ";") global-vars))
	    (string-append (generate var) " = " (generate val) "\n")]
	   [`(= ,rator ,rand)
	    (string-append "ValueEqual(" (generate rator) ", " (generate rand) ")")]
	   [('locate bind body)
	    (string-append 
	     (split (map (lambda (x) 
			   (case (cadr x)
			     ['CLOSURE (string-append "struct Closure " (symbol->string (car x)) ";")]
			     ['ENV (let ((var (symbol->string (car x)))
					 (size (number->string (caddr x))))
				     (string-append "struct Env " var ";\n"
						    var ".value = alloca(sizeof(Value)*" size ");"))]
			     ['VECTOR (let ((var (symbol->string (car x)))
					    (size (number->string (caddr x))))
					(string-append "struct Vector " var ";\n"
						       var ".value = alloca(sizeof(Value)*" size ");"))]
			     ['CONS (string-append "struct Cons " (symbol->string (car x)) ";")]))
			 bind) "\n")
	     "\n" (generate body))]
	   [('lambda bind body)
	    (generate-lambda bind body
			     (lambda (func-name declear def)
			       (set! global-funcs (cons def global-funcs))
			       func-name))]
	   [('InitClosure addr lam env)
	    (string-append "InitClosure(&" (symbol->string addr) ", " (generate lam) ", " (generate env) ")")]
	   [('InitVector addr n v ...)
	    (string-append "InitVector(&" (symbol->string addr) ", " (number->string n) ", " (split (map generate v) ", ") ")")]
	   [('InitEnv addr n v ...)
	    (string-append "InitEnv(&" (symbol->string addr) ", " (number->string n) ", " (split (map generate v) ", ") ")")]
	   [(rator rand ...)
	    (if (prim? rator)
		(string-append (generate rator) "(" (split (map generate rand) ", ") ")")
		(let ((tmp (string-append "((struct Closure*)" (generate rator) ")")))
		  (string-append tmp "->lam(" tmp "->env, " (split (map generate rand) ", ") ")")))])))

;; 接受一个lambda表达式，对里面的所有涉及到分配的操作拆分成分配空间和使用数据
;; (lambda (x)
;;    (cons x a)
;;    (vector 6)) =>
;; (lambda (x)
;;    (locate ((tmp1 CONS 2)
;;             (tmp2 VECTOR 6))
;;            (InitCons tmp1 x a)
;;            (InitVector tmp2 6))
(define explicit-allocation
  (lambda (exp)
    (explicit-alloc exp 
		    (lambda (e b)
		      (if (null? b)
			  e
			  `(locate ,b ,e))))))

;; 接受一个exp和一个连续，返回exp和提取出来的分配
(define explicit-alloc
  (lambda (exp cont)
    (match exp
	   [(? symbol?) (cont exp '())]
	   [(? integer?) (cont exp '())]
	   [`(if ,test ,then ,else)
	    (explicit-alloc 
	     test
	     (lambda (test$ b1)
	       (explicit-alloc
		then
		(lambda (then$ b2)
		  (explicit-alloc
		   else
		   (lambda (else$ b3)
		     (cont `(if ,test$ ,then$ ,else$) (append b1 b2 b3))))))))]
	   [('begin es ...)
	    (explicit-alloc-list es 
				 (lambda (es$ b)
				   (cont `(begin ,@es$) b)))]
	   [`(set! ,var ,val)
	    (explicit-alloc val
			    (lambda (val$ b)
			      (cont `(set! ,var ,val$) b)))]
	   [`(define ,var ,val)
	    (explicit-alloc val
			    (lambda (val$ b)
			      (cont `(define ,var ,val$) b)))]
	   [('lambda bind body)
	    (explicit-alloc body
			    (lambda (body$ b)
			      (if (null? b)
				  (cont exp '())				  
				  (cont
				   `(lambda ,bind
				      (locate ,b ,body$)) '()))))]
	   [`(cons ,x ,y)
	    (let ((tmp (gensym 'tmp)))
	      (explicit-alloc x
			      (lambda (x$ b1)
				(explicit-alloc y
						(lambda (y$ b2)						  
						  (cont `(InitCons ,tmp ,x$ ,y$) 
							(append (cons (list tmp 'CONS 2) b1) b2)))))))]
	   [('env-make num fvs ...)
	    (let ((tmp (gensym 'tmp)))
	      (cont `(InitEnv ,tmp ,num ,@fvs) (list (list tmp 'ENV num))))]
	   [('closure lam env)
	    (let ((tmp (gensym 'tmp)))
	      (explicit-alloc lam
			      (lambda (lam$ b)
				(explicit-alloc env
						(lambda (env$ b1)
						  (cont `(InitClosure ,tmp ,lam$ ,env$)
							(append (cons (list tmp 'CLOSURE 2) b1) b)))))))]
	   [(rator rand ...)
	    (explicit-alloc-list rand
				 (lambda (rand$ bind)
				   (cont `(,rator ,@rand$) bind)))])))

(define explicit-alloc-list
  (lambda (lst+ cont)
    (explicit-alloc
     (car lst+)
     (lambda (e b)
       (if (null? (cdr lst+))
	   (cont (cons e '()) b)
	   (explicit-alloc-list (cdr lst+)
				(lambda (remain b1)
				  (cont (cons e remain) (append b b1)))))))))
