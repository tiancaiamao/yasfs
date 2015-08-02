(define-record-type :thread
  (make-:thread name state specific cont)
  thread?
  (name thread-name)
  (state :thread-state :thread-state-set!)
  (specific thread-specific thread-specific-set!)
  (cont :thread-cont :thread-cont-set!))

(define ready-head '())
(define ready-tail '())

(define (ready-empty?)
  (and (eq? ready-head '())
       (eq? ready-tail '())))

(define (ready-enqueue v)
  (if (null? ready-tail)
      (begin
	(set! ready-head (cons v '()))
	(set! ready-tail ready-head))
      (begin
	(let ((node (cons v '())))
	  (set-cdr! ready-tail node)
	  (set! ready-tail node)))))

(define (ready-dequeue)
  (let ((ret (car ready-head)))
    (set! ready-head (cdr ready-head))
    (if (null? ready-head) (set! ready-tail '()))
    ret))

(define this #f)

(define (current-thread)
  this)

(define (make-thread thunk . name)
  (set! name (or (symbol? name) (gensym 'thread)))
  (make-:thread name 'new (begin)
		(lambda (_)
		  (thunk))))

(define (current/cc)
  (call/cc (lambda (cc) cc)))

(define (thread-yield!)
  (if (ready-empty?)
      (schedule)))

(define (schedule)
  (let loop ()     
    (let* ((next (ready-dequeue))
	   (nc (:thread-cont next)))
      (:thread-state-set! this 'ready)
      (ready-enqueue this)
      (:thread-state-set! next 'running)
      (let ((cc (current/cc)))
	(if (procedure? cc)
	    (begin 
	      (:thread-cont-set! this cc)
	      (set! this next)
	      (nc 'resume))
	    (loop))))))

(define (thread-start! thread)
  (or (and (thread? thread) 
	   (eq? (:thread-state thread) 'new))
      (error "not a valid thread!"))
  (:thread-state-set! thread 'ready)
  (ready-enqueue thread)
  (schedule))

(define (thread-terminate! thread)
  (if (eq? thread this)
      (let* ((next (ready-dequeue))
	     (nc (:thread-cont next)))
	(set! this next)
	(nc 'resume))))

(set! this (make-thread '()))
(:thread-state-set! this 'running)


(define counter 10)
(define (make-thread-thunk name)
  (letrec ((loop (lambda ()
		   (and (>= counter 0)
			(begin
			  (display "in thread ")
			  (display name)
			  (display "; counter = ")
			  (display counter)
			  (newline)
			  (set! counter (- counter 1))
			  (thread-yield!)
			  (loop))))))
    loop))

(begin (thread-start! (make-thread (make-thread-thunk 'a)))
       (thread-start! (make-thread (make-thread-thunk 'b)))
       (thread-start! (make-thread (make-thread-thunk 'c))))

