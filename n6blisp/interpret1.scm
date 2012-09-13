(define (my-write obj)
  (cond ((and (pair? obj) (equal? (car obj) '<use-procedure>))
	 (display "<procedure>"))
	((and (pair? obj) (equal? (car obj) '<macro>))
	 (display "<macro>"))
	((pair? obj) 
	 (display "unsafe to display pair"))
	(else
	 (write obj))))
(define (repl evaluator env)
  (display "repl>")
  (let ((expr (read)))
    (cond ((eof-object? expr)
	   (display "byebye")
	   (newline))
	  (else
	   (my-write (evaluator expr env))
	   (newline)
	   (repl evaluator env)))))

(define (my-eval expr envt)
  (cond ((symbol? expr)
	 (eval-symbol expr envt))
	((pair? expr)
	 (eval-list expr envt))
	((self-evaluating? expr)
	 expr)
	(else
	 (error "Illegal expression form" expr))))

(define (eval-symbol name env)
  (lookup-variable name env))

(define (lookup-variable name env)
  (if (null? env)
      (error "unbound value in env")
      (let ((find (assoc name (cdr env))))
	(if find
	    (cdr find)
	    (lookup-variable name (car env))))))

(define (eval-list ls env)
  (let ((op (lookup-variable (car ls) env)))
    (cond 
     ((core-form? op)
      ((cdr op) ls env))
     ((macro? op)
      (eval-macro-call (cdr op) ls env))
     ((use-procedure? op)
      (my-apply op
		(map (lambda (o) (my-eval o env)) (cdr ls))))
     ((procedure? op)   ;;primitive 
      (apply op (map (lambda (o) (my-eval o env)) (cdr ls))))
     (else
      (error "must be core-form/procedure/macro: " (car ls))))))

(define (macro? op)
  (and (pair? op) (equal? (car op) '<macro>)))

(define (core-form? pair)
  (and (pair? pair) (equal? (car pair) '<core-form>)))

(define (use-procedure? pair)
  (and (pair? pair) (equal? (car pair) '<use-procedure>)))

(define (eval-macro-call transformer expr envt)
  (my-eval (my-apply transformer expr) envt))

(define (self-evaluating? expr)
  (or (number? expr) (boolean? expr) (string? expr)))

(define (define-variable name value env)
  (let ((find (assoc name (cdr env))))
    (if find
	(set-cdr! find value)
	(set-cdr! env (cons (cons name value) (cdr env))))))

(define (eval-define exp env)
  (let ((value (my-eval (caddr exp) env)))
    (define-variable (cadr exp) value env)))

(define (eval-define-rewriter exp env)
  (let ((value (my-eval (caddr exp) env)))
    (define-variable (cadr exp) (cons '<macro> value) env)))

(define (eval-set! exp env)
  (let ((find (assoc (cadr exp) (cdr env))))
    (if find
	(set-cdr! find (my-eval (caddr exp) env))
	(error "can't set! a unbound value"))))
(define (eval-quote exp env)
  (cadr exp))

(define (eval-lambda exp env)
  (let ((args (cadr exp))
	(body (cons 'begin (cddr exp))))
    (make-closure env args body)))

(define (eval-begin exp env)
  (cond ((null? (cdr exp)) '())
	((null? (cddr exp)) (my-eval (cadr exp) env))
	(else
	 (eval-begin (cdr exp) env))))

(define (make-closure env args body)
  (list '<use-procedure> env args body))

(define (my-apply proc arg)
  (let* ((parent (cadr proc))
	 (args (caddr proc))
	 (body (cadddr proc))
	 (env (cons parent (map cons args arg))))
    (my-eval body env)))

(define (init-env)
  (let ((ret (cons '() '())))
    (define-variable 'define (cons '<core-form> eval-define) ret)
    (define-variable 'define-rewriter (cons '<core-form> eval-define-rewriter) ret)
    (define-variable 'set! (cons '<core-form> eval-set!) ret)
    (define-variable 'quote (cons '<core-form> eval-quote) ret)
    (define-variable 'lambda (cons '<core-form> eval-lambda) ret)
    (define-variable 'begin (cons '<core-form> eval-begin) ret)
    (define-variable '+ + ret)
    (define-variable 'cons cons ret)
    ret))