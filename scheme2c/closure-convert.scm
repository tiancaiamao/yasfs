(define (tagged-list? e tag)
  (and (pair? e) (eq? (car e) tag)))

(define (lambda? e) (tagged-list? e 'lambda))
(define (lambda->bind e) (cadr e))
(define (lambda->body e) (caddr e))

(define (if? e) (tagged-list? e 'if))
(define (if->test e) (cadr e))
(define (if->then e) (caddr e))
(define (if->else e) (caddr (cdr e)))

(define (set!? e) (tagged-list? e 'set!))
(define (set!->var e) (cadr e))
(define (set!->val e) (caddr e))

(define difference
  (lambda (lst1 lst2)
    (if (not (pair? lst2))
	lst1
	(difference (remove (car lst2) lst1) (cdr lst2)))))

(define environments '())
(define num-envs 0)
(define allocate-environment
  (lambda (lst)
    (set! num-envs (+ num-envs 1))
    (set! environments (cons (cons num-envs lst) environments))
    num-envs))

(define closure-convert
  (lambda (exp)
    (cond
     ((or (symbol? exp) (boolean? exp) (number? exp))
      exp)
     ((lambda? exp) 
      (let* ((body (closure-convert (lambda->body exp)))
	     (fv (difference (free-vars body) (lambda->bind exp)))
	     (id (allocate-environment fv))
	     ($env (gensym 'env))
	     (sub (map (lambda (v)
			 (list v `(env-get ,id ,v ,$env)))
		       fv)))
	`(closure (lambda (,$env ,@(lambda->bind exp))
		    ,(substitute sub body))
		  (env-make ,id ,@(azip fv fv)))))
     ((if? exp)
      `(if ,(closure-convert (if->test exp))
	   ,(closure-convert (if->then exp))
	   ,(closure-convert (if->else exp))))
     ((set!? exp)
      `(set! ,(set!->var exp)
	     ,(closure-convert (set!->val exp))))
     ((app? exp)
      (map closure-convert exp))
     (else (error "unhandled exp: " exp)))))