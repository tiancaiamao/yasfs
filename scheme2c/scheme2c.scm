(define scheme2c
  (lambda (exps)
    (set! global-funcs '())
    (set! global-vars '())
    (map (lambda (exp)
	   (generate
	    (explicit-allocation
	     (closure-convert 
	      (T-c exp 'c0)))))
	 exps)))
