(define (trivial? x) (or (number? x) (symbol? x) (string? x) (boolean? x)))
(define (void) (begin))

;; M变换，处理trivial和lambda表达式
(define M
  (lambda (exp)
    (match exp
	   [(? trivial?) exp]
	   [`(lambda (,x ...) ,e)
	    (let ((k$ (gensym 'k)))
	      `(lambda (,@x ,k$)
		 ,(T-c e k$)))])))

;; sexp x label => sexp
(define T-c
  (lambda (exp c)
    (match exp
	   [(? trivial?) `(,c ,(M exp))]
	   [`(begin ,e) (T-c e c)]
	   [('begin e es ...)
	    (T-k e (lambda (_)
		     (T-c `(begin ,@es) c)))]
	   [`(if ,test ,then ,else)
	    (let ((k (gensym 'k$)))
	      `((lambda (,k)
		  ,(T-k test 
			(lambda (test$)
			  `(if ,test$
			       ,(T-c then k)
			       ,(T-c else k)))))
		,c))]
	   [`(set! ,var ,val)
	    (T-k val
		 (lambda (val$)
		   `(set/k! ,var ,val$
			    (,c (void)))))]
	   [(f es ...)
	    (T-k f 
		 (lambda (f$)
		   (T*-k es
			 (lambda (es$)
			   `(,f$ ,@es$ ,c)))))])))

;; sexps x (list => sexp) => sexp
;; 参数k是接受一个list，返回一个sexp
(define T*-k
  (lambda (exps k)
    (if (null? exps)
	(k '())
	(T-k (car exps)
	     (lambda (first)
	       (T*-k (cdr exps)
		     (lambda (remain)
		       (k (cons first remain)))))))))

;; sexp x (sexp => sexp) => sexp
(define T-k
  (lambda (exp k)
    (match exp
	   [(? trivial?) (k (M exp))]	   
	   [`(begin ,e)
	    (T-k e k)]
	   [('begin e es ...)
	    (T-k e (lambda (_)
		     (T-k `(begin ,@es) k)))]
	   [`(if ,test ,then ,else)
	    (let* ((rv (gensym 'rv$))
		   (cont `(lambda (,rv) ,(k rv))))	      
	      (T-k test
		   (lambda (test$)
		     `(if ,test$
			  ,(T-c then cont)
			  ,(T-c else cont)))))]
	   [`(set! ,var ,val)
	    (T-k val
		 (lambda (val$)
		   `(set/k! ,var ,val
			    ,(k '(void)))))]
	   [(f es ...)
	    (let* ((rv (gensym 'rv$))
		   (cont `(lambda (,rv) ,(k rv))))
	      (T-c exp cont))])))

