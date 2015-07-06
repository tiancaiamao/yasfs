(module sandbox
	(make-sandbox
	 sandbox-ref
	 sandbox-set!
	 sandbox-remove!
	 sandbox-eval)
	
	(import scheme chicken)

	(include "desugar.scm")

	(define (filter pred lst)
	  (define (filter1 f l ret)
	    (if (null? l)
		ret
		(if (f (car l))
		    (filter1 f (cdr l) ret)
		    (filter1 f (cdr l) (cons (car l) ret)))))
	  (filter1 pred lst '()))

	(define (posq x lst)
	  (let loop ((lst lst) (i 0))
	    (cond ((null? lst) #f)
		  ((eq? x (car lst)) i)
		  (else (loop (cdr lst) (+ i 1))) ) ) )

	(define (lookup var e)
	  (let loop ((envs e) (ei 0))
	    (cond ((null? envs) (values #f var))
		  ((posq var (car envs)) => (lambda (p) (values ei p)))
		  (else (loop (cdr envs) (+ ei 1))) ) ) )

	(define (defined? var e)
	  (receive (i j) (lookup var e) i) )

	(define (undefine vars e)
	  (let loop ([envs e])
	    (if (null? envs)
		'()
		(let ([envi (car envs)])
		  (cons
		   (let delq ([ee envi])
		     (if (null? ee)
			 '()
			 (let ([h (car ee)]
			       [r (cdr ee)] )
			   (if (memq h vars)
			       r
			       (cons h (delq r)) ) ) ) )
		   (loop (cdr envs)) ) ) ) ) )

	(define (s-error loc msg . args)
	  (signal
	   (make-composite-condition
	    (make-property-condition 'sandbox)
	    (make-property-condition
	     'exn
	     'location loc
	     'message msg
	     'arguments args) ) ) )

	(define (make-sandbox)
	  (let ([env '()])
	    (lambda (method)
	      (case method
		((set!)
		 (lambda (id val)
		   (set! env (cons (cons id val) env))))
		((ref)
		 (lambda (id)
		   (assq id env)))
		((remove!)
		 (lambda (id)
		   (set! env
			 (filter (lambda (x) (eq? (car x) id)) env))))
		((debug)
		 (lambda ()
		   env))))))

	(define (environment-set! e id val)
	  ((e 'set!) id val))
	(define (environment-ref e id)
	  ((e 'ref) id))
	(define (environment-remove! e id)
	  ((e 'remove!) id))
	(define (environment-debug e)
	  ((e 'debug)))

	;; for export
	(define sandbox-set! environment-set!)
	(define sandbox-ref
	  (lambda (sandbox var)
	    (let ([a (environment-ref sandbox var)])
	      (if a (cdr a)))))
	(define sandbox-remove! environment-remove!)

	(define (compile-call x e sandbox)
	  (let* ([fn (compile (car x) e sandbox)]
		 [args (cdr x)])
	    (let ([as (map (lambda (a) (compile a e sandbox)) args)])
	      (lambda (v)
		(apply (fn v) (map (lambda (a) (a v)) as))) )))

	(define (compile x e sandbox)
	  (cond 
	   [(symbol? x)
	    (let-values ([(i j) (lookup x e)])
	      (cond 
	       [(not i)
		(let ([a (environment-ref sandbox x)])	  
		  (if (not a)
		      (lambda v
			(s-error #f "unbound variable" x))
		      (lambda v (cdr a))))]
	       [(zero? i) (lambda (v) (vector-ref (car v) j))]
	       [else (lambda (v) (vector-ref (list-ref v i) j))] ) ) ]
	   [(number? x) (lambda v x)]
	   [(boolean? x)
	    (if x (lambda v #t) (lambda v #f) ) ]
	   [(or (char? x) (eof-object? x) (string? x) )
	    (lambda v x) ]
	   [(symbol? (car x))
	    (let ([head (car x)])
	      (if (defined? head e)
		  (compile-call x e sandbox)
		  (case head
		    [(quote)
		     (lambda v (cadr x)) ]
		    [(if)
		     (let* ([test (compile (cadr x) e sandbox)]
			    [cns (compile (caddr x) e sandbox)]
			    [alt (compile (cadddr x) e sandbox)] )
		       (lambda (v) (if (test v)
				       (cns v)
				       (alt v))) ) ]
		    [(begin)
		     (let ([body (cdr x)])
		       (if (pair? body)
			   (if (null? (cdr body))
			       (compile (car body) e sandbox)
			       (let* ([x1 (compile (car body) e sandbox)]
				      [x2 (compile `(begin ,@(cdr body)) e sandbox)])
				 (lambda (v) 
				   (x1 v)
				   (x2 v)) )))) ]
		    [(set!)
		     (let ([var (cadr x)])
		       (let-values ([(i j) (lookup var e)])
			 (let ([val (compile (caddr x) e sandbox)])
			   (cond 
			    [(not i)
			     (let ([a (environment-ref sandbox var)])
			       (if (not a)
				   (lambda (v) (environment-set! sandbox var (val v)))
				   (lambda (v) (set-cdr! a (val v)))))]
			    [(zero? i)
			     (lambda (v) (vector-set! (car v) j (val v)))]
			    [else
			     (lambda (v)
			       (vector-set! (list-ref v i) j (val v)) ) ] ) ) ) ) ]
		    [(let)
		     (let* ([bindings (cadr x)]
			    [n (length bindings)]
			    [vars (map (lambda (x) (car x)) bindings)]
			    [body (compile
				   (cons 'begin (cddr x))
				   (cons vars e)
				   sandbox) ] )
		       (let ([vals (map 
				    (lambda (x)
				      (compile (cadr x) e sandbox))
				    bindings)])
			 (lambda (v)
			   (let ([v2 (make-vector n)])
			     (do ([i 0 (+ i 1)]
				  [vlist vals (cdr vlist)] )
				 ((>= i n))
			       (vector-set! v2 i ((car vlist) v)) )
			     (body (cons v2 v)) ) ) ) ) ]
		    [(lambda)
		     (let* ((vars (cadr x))
			    (body (cddr x))
			    (argc (length vars)))
		       (let ([body (compile
				    (cons 'begin body)
				    (cons vars e) 
				    sandbox) ] )
			 (lambda (v)
			   (lambda as
			     (body (cons (apply vector as) v)) ))))]
		    [else (compile-call x e sandbox)] ) ) ) ]
	   [else (compile-call x e sandbox)] ) )

	(define (sandbox-eval exp sandbox)
	  (set! exp (desugar exp))
	  ((compile exp '() sandbox) '()))
	)
