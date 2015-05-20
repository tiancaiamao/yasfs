(define scheme2c
  (lambda (exp)
    (set! global-funcs '())
    (set! global-vars '())
    (let ((content   (generate
		      (explicit-allocation
		       (closure-convert 
			(T-c 
			 (lift-inner exp) 'cont))))))
      (map print global-vars)
      (map print global-funcs)
      (print (string-append "void TopLevel(Value cont) {\n"
			    "CheckMinorGC(cont);\n"
			    content
			    "}\n")))))
