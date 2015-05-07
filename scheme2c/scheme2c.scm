(define scheme2c
  (lambda (exps)
    (set! global-funcs '())
    (set! global-vars '())
    (let ((content (map (lambda (exp)
			  (generate
			   (explicit-allocation
			    (closure-convert 
			     (T-c exp 'cont)))))
			exps)))      
      (map print global-vars)
      (map print global-funcs)
      (print (string-append "void TopLevel(Value cont) {\n"
			    "CheckMinorGC(cont);\n"
			    (split content "\n")
			    "}\n")))))
