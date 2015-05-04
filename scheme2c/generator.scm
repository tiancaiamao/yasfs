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
	   [`(= ,rator ,rand)
	    (string-append "ValueEqual(" (generate rator) ", " (generate rand) ")")]
	   [('lambda bind body)
	    (generate-lambda bind body
			     (lambda (func-name declear def)
			       (set! global-funcs (cons def global-funcs))
			       func-name))]
	   [('closure lam env)
	    (string-append "MakeClosure(" (generate lam) ", " (generate env) ")")]
	   [(rator rand ...)
	    (if (prim? rator)
		(string-append (generate rator) "(" (split (map generate rand) ", ") ")")
		(let ((tmp (string-append "((struct Closure*)" (generate rator) ")")))
		  (string-append tmp "->lam(" tmp "->env, " (split (map generate rand) ", ") ")")))])))
