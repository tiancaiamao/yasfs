(define (prim? x) (memq x '(+ - * / =)))

(define split 
  (lambda (lst c)
    (cond
     ((null? lst) "")
     ((null? (cdr lst)) (car lst))
     (else 
      (string-append (car lst) c (split (cdr lst) c))))))

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
		  ['* "__product"])		  
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
	   [(rator rand ...)
	    (if (prim? rator)
		(string-append (generate rator) "(" (split (map generate rand) ", ") ")")
		(string-append "((struct Closure*)" (generate rator) ")->lam(" (split (map generate rand) ", ") ")"))])))

(print (generate '(set! a (begin 1 (if #t 2 3) 3 4))))

(print (generate '(if (= n 0)
	       1
	       (* n (fact (- n 1))))))


