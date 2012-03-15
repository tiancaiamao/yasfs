(define basic-cont (lambda (v) v))
;;----------------------------------------
(define (atom? o) (not (pair? o)))
(define (make-cont num)
  (string->symbol (string-append "k" (number->string num))))
(define (cps-convert exp level)
    (if (atom? exp)
	(let ((cc (make-cont))) `(,cc ,exp))
	(case (car exp)
	  ((if) (cps-convert-if (cdr exp) level))
	  ((set!) (cps-convert-set (cdr exp) level))
	  ((lambda) (cps-convert-lambda (cdr exp) level))
	  ((begin) (cps-convert-begin (cdr exp) level))
	  (else (format "unsupported:~A" exp)))))
;;---------------------------------失败 没搞清楚展开后应该是什么样子
(define (cps-convert-if exp level)
  (let ((test (car exp))
	(etrue (cadr exp))
	(efalse (caddr exp)))
    `((lambda (v) 
	(if v
	    ,(cps-convert etrue)
	    ,(cps-convert efalse)))
      ,(cps-convert test))))
(define (cps-convert-set exp level)
  (let ((name (car exp))
	(value (cadr exp)))
    `((lambda (v) 
	(cc (set! ,name v)))
      ,(cps-convert value (+ level 1)))))
(define (cps-convert-begin exp level)
  (cond ((pair? exp)
	 (if (null? (cdr exp))
	     (cps-convert exp)
	     `((lambda (v) ,(cps-convert-begin (cdr exp)))
	   ,(cps-convert (car exp)))))
	((null? exp)
	 `(cc scheme-null-object))
	(else (cps-convert exp))))
	
(define (cps-convert-lambda exp)
  (let ((

(define (cps-convert exp nocc?)
    (if (atom? exp)
	(if nocc?
	    exp
	    `(cc exp))
	(case (car exp)
	  ((if) (cps-convert-if (cdr exp) nocc?))
	  ((set!) (cps-convert-set (cdr exp) nocc?))
	  ((lambda) (cps-convert-lambda (cdr exp) nocc?))
	  ((begin) (cps-convert-begin (cdr exp) nocc?))
	  (else (format "unsupported:~A" exp)))))
(define (cps-convert-set exp nocc?)
  (let ((name (car exp))
	(value (cadr exp)))
    (display name)
    (newline)
    `((lambda (v) 
	,(if nocc?
	    `(set! ,name v)
	    `(cc (set! ,name v))))
      ,(cps-convert value #t))))

(define (cps-convert-begin exp nocc?)
  (cond ((pair? exp)
	 (if (null? (cdr exp))
	     (cps-convert exp nocc?)
	     `((lambda (v) ,(cps-convert-begin (cdr exp) nocc?))
	   ,(cps-convert (car exp) #t))))
	((null? exp)
	 (if
	  nocc?
	  scheme-null-object
	 `(cc scheme-null-object)))
	(else (error "wrong begin"))))

;;----------------------------------------------------------
#lang racket
(define (atom? exp) (not (pair? exp)))
;;-----失败 nocc?不能解决问题，有好多种不同的连续，必须在内部展开
;;所以要把连续作为参数加到cps-convert中
(define (cps-convert exp nocc?)
  (if (atom? exp)
      (if nocc?
	  exp
	  `(cc ,exp))
      (case (car exp)
        ((set!) (cps-convert-set (cdr exp) nocc?))
        ((if) (cps-convert-if (cdr exp) nocc?))
        ((begin) (cps-convert-begin (cdr exp) nocc?))
        ((lambda) (cps-convert-lambda (cdr exp) nocc?))
        (else 
         (cps-convert-application exp nocc?)))))
(define (make-args number)
  (let loop ((i 0)
             (lst '()))
    (let ((v (string->symbol (string-append "v" (number->string i)))))
      (if (= i number)
          (reverse lst)
          (loop (+ i 1) (cons v lst))))))
  
(define (cps-convert-argument func args number nocc?)
  (if (null? args)
      (if nocc?
          `(,func ,@(make-args number))
          `(cc (,func ,@(make-args number))))
      `((lambda ,(list (string->symbol (string-append "v" (number->string number))))
         ,(cps-convert-argument func (cdr args) (+ number 1) nocc?))
        ,(cps-convert (car args) #t))))
  
  
(define (cps-convert-application exp nocc?)
  (cps-convert-argument (car exp) (cdr exp) 0 nocc?))

(define (cps-convert-set exp nocc?)
  (let ((name (car exp))
        (value (cadr exp)))
    `((lambda (v)
        ,(if nocc?
             `(set! ,name v)
             `(cc (set! ,name v))))
      ,(cps-convert value #t))))

(define (cps-convert-if exp nocc?)
  (let ((test (car exp))
        (etrue (cadr exp))
        (efalse (caddr exp)))
    `((lambda (v)
        (if v
            ,(cps-convert etrue nocc?)
            ,(cps-convert efalse nocc?)))
      ,(cps-convert test #t))))

(define (cps-convert-begin exp nocc?)
  (if (pair? exp)
      (if (null? (cdr exp))
          (cps-convert (car exp) nocc?)
          `((lambda (v)
              ,(cps-convert-begin (cdr exp) nocc?))
            ,(cps-convert (car exp) #t)))
      (if (not (null? exp))
          (error "wrong form of begin")
          (if nocc?
              'scheme-null-object
              '(cc scheme-null-object)))))
(define (cps-convert-lambda exp nocc?)
  (let ((args (car exp))
        (body (cdr exp)))
    (if nocc?
        `(lambda ,(cons 'k args) ,(cons 'k body))
        `(cc (lambda ,(cons 'k args) ,(cons 'k body))))))