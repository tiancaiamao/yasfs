(define (atom? o)
  (not (pair? o)))
(define (evaluate e env cont)
  (if (atom? e)
      (if (symbol? e) 
	  (evaluate-variable e env cont)
          (cont e))
      (case (car e)
	((define) (evaluate-define (cadr e) (caddr e) env cont))
        ((quote)  (evaluate-quote (cadr e) env cont))
        ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) env cont))
        ((begin)  (evaluate-begin (cdr e) env cont))
	((set!)   (evaluate-set! (cadr e) (caddr e) env cont))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) env cont))
        (else     (evaluate-application (car e) (cdr e) env cont)))))
(define (evaluate-variable name env cont)
  (cont (let ((find (lookup-env-cell env name)))
	  (if find
	      (cdr find)
	      (error "unbound variable in environment")))))
(define (evaluate-quote e env cont)
  (cont e))
(define (lookup-env-cell env name)
  (if (pair? env)
      (let ((parent (car env))
	    (binding (cdr env)))
	(or (assv name binding) (lookup-env-cell parent name)))
      #f))
(define (evaluate-if test et ef env cont)
  (evaluate test env 
	    (lambda (v)
		  (evaluate (if v et ef) env cont))))
(define (evaluate-begin e* env cont)
  (if (pair? (cdr e*))
      (evaluate (car e*) env 
		(lambda (ignore)
		  (evaluate-begin (cdr e*) env cont)))
      (evaluate (car e*) env cont)))
(define (evaluate-set! name value env cont)
  (evaluate value env 
	    (lambda (v)
	      (let ((env-cell (lookup-env-cell env name)))
		      (if env-cell
			  (cont (set-cdr! env-cell v))
			  (error "can't set unbound variable"))))))
(define (evaluate-define name value env cont)
  (evaluate value env
	    (lambda (v)
	      (if (pair? env)
		  (let* ((binding (cdr env))
			 (find (assv name binding)))
		    (if find 
			(cont (set-cdr! find value))
			(cont (set-cdr! env (cons (cons name value) binding)))))
		  (error "can't in null-env")))))

(define (extend-env n* v* env)
  (let ((binding (map cons n* v*)))
    (cons env binding)))
(define (evaluate-lambda n* e* env cont)
  (cont 
   (lambda (v* runtime-cont)
     (let ((new-env (extend-env n* v* env)))
       (runtime-cont (evaluate-begin e* new-env runtime-cont))))))
(define (evaluate-application f e* env cont)
  (define (evaluate-argument e* env cont)
    (if (pair? e*)
        (evaluate (car e*) env
		  (lambda (v)
		    (evaluate-argument (cdr e*) env
					(lambda (v*)
					  (cont (cons v v*)) ) ) ) )
        (cont '()) ) )
  (evaluate-argument e* env 
		     (lambda (v*)
		       (f v* cont))))


(define global-env (cons '() '()))
(define toplevel-cont (lambda (v) v))

(define-syntax define-primitive
  (syntax-rules ()
    ((_ name primitive arity)
     (evaluate-define 'name 
		      (lambda (v* cont)
			(if (= arity (length v*))
			    (cont (apply primitive v*))
			    (error "incorrect arity")))
		      global-env toplevel-cont))))

(define-primitive cons cons 2)
(define-primitive + + 2)

(define (interpret)
      (write (evaluate (read) global-env toplevel-cont))
      (interpret))