(define (trivial? x) (or (symbol? x) (number? x) (string? x)))

;; M变换只处理原子类型和lambda表达式
(define M
  (lambda (exp)
    (match exp
	   [(? trivial?) exp]
	   [`(lambda (,var) ,e)
	    (let ((k$ (gensym 'k$)))
	      `(lambda (,var ,k$)
		 ,(T-c e k$)))])))

;; T-c: sexp x symbol -> sexp
(define T-c
  (lambda (exp c)
    (match exp
	   [(? trivial?) `(,c ,(M exp))]
	   [`(lambda (,var) ,e) `(,c ,(M exp))]
	   [`(,f ,e)
	    (T-k f
		 (lambda (f$)
		   (T-k e
			(lambda (e$)
			  `(,f$ ,e$ ,c)))))])))

;; T-k: sexp x (sexp -> sexp) -> sexp
(define T-k
  (lambda (exp k)
    (match exp
	   [(? trivial?) (k (M exp))]
	   [`(lambda (,var) ,e) (k (M exp))]
	   [`(,f ,e)
	    (let* ((rv$ (gensym 'rv$))
		   (cont `(lambda (,rv$) ,(k rv$))))
	      (T-k f
		   (lambda (f$)
		     (T-k e
			  (lambda (e$)
			    `(,f$ ,e$ ,cont))))))])))
    


(T-c '(lambda (x)
	(lambda (y)
	  (lambda (z)
	    (x (y z))))) 'halt)

(T-k '(g a) (lambda (x) x))
