(define-record-type :thread
  (make-:thread name state specific cont)
  thread?
  (name thread-name :thread-name-set!)
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
      (let ((node (cons v '())))
	(set-cdr! ready-tail node)
	(set! ready-tail node))))

(define (ready-dequeue)
  (let ((ret (car ready-head)))
    (set! ready-head (cdr ready-head))
    (if (null? ready-head) (set! ready-tail '()))
    ret))

(define (ready-delete v)
  ;; prev not null
  (define (delete-help prev cur v)
    (cond
     ((null? cur) void)
     ((eq? (car cur) v)
      (begin 
	(set-cdr! prev (cdr cur))
	(if (eq? cur ready-tail)
	    (set! ready-tail prev))))
     (else
      (delete-help cur (cdr cur) v))))

  (cond
   ((null? ready-head) void)
   ((eq? (car ready-head) v)
    (if (eq? ready-head ready-tail)
	(begin (set! ready-head '())
	       (set! ready-tail '()))
	(set! ready-head (cdr ready-head))))
   (else
    (delete-help ready-head (cdr ready-head) v))))

(define this #f)
(define void (begin))
(define (current-thread)
  this)

;; switch to another thread
(define (switch-to to)
  (:thread-state-set! to 'running)
  (set! this to)
  ((:thread-cont this) void))

(define (thread-exit-hook)
  (:thread-state-set! this 'dead)
  (or (ready-empty?)
      (let ((next (ready-dequeue)))
	(switch-to next))))

(define (make-thread thunk . name)
  (set! name (or (symbol? name) (gensym 'thread)))
  (make-:thread name 'new void
		(lambda (_)
		  (thunk)
		  (thread-exit-hook))))

;; save current thread and switch to another thread
(define (switch to)
  (call/cc
   (lambda (save)
     (:thread-cont-set! this save)
     (:thread-state-set! this 'ready)
     (ready-enqueue this)
     (switch-to to))))

;; take a appropriate thread and switch to it
(define (schedule)
  (let ((next (ready-dequeue)))
    (switch next)))

(define (thread-yield!)
  (if (not (ready-empty?)) (schedule)))

;; put thread to ready queue, also trigger a schedule
(define (thread-start! thread)
  (or (and (thread? thread) 
	   (eq? (:thread-state thread) 'new))
      (error "not a valid thread!"))
  (:thread-state-set! thread 'ready)
  (ready-enqueue thread)
  (schedule))

(define (thread-terminate! thread)
  (:thread-state-set! thread 'dead)
  (cond
   ((eq? thread this) ;;suicide
    (if (ready-empty?) (exit) (thread-exit-hook)))
   (else
    (ready-delete thread))))
    

(set! this (make-thread '()))
(:thread-state-set! this 'running)
(:thread-name-set! this 'primordial)
(call/cc (lambda (cc)
	   (:thread-cont-set! this cc)))


(define t (make-thread 
	   (lambda ()
	     (printf "print a line ~A~%")
	     (thread-yield!)
	     (printf "after yield~%")
	     (thread-yield!)
	     (printf "after after yield ~%")
	     (printf "finish"))))


(:thread-name-set! t 'create-thread)
(:thread-state t)
(eq? t this)
((:thread-cont this) 'resume)

(thread-start! t)
(thread-name (car ready-head))
(thread-name t)
(thread-yield!)
      


