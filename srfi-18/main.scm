(define thread-queue '())

(define halt #f)

(define (void) (begin))

(define (current/cc)
  (call/cc (lambda (cc) cc)))

(define (spawn thunk)
  (let ((cc (current/cc)))
    (if (procedure? cc)
	(set! thread-queue (append thread-queue (list cc)))
	(begin (thunk) (quit)))))

(define (yield)
  (let ((cc (current/cc)))
    (if (and (procedure? cc) (pair? thread-queue))
	(let ((next-thread (car thread-queue)))
	  (set! thread-queue (append (cdr thread-queue) (list cc)))
	  (next-thread 'resume))
	(void))))

(define (quit)
  (if (pair? thread-queue)
      (let ((next-thread (car thread-queue)))
	(set! thread-queue (cdr thread-queue))
	(next-thread 'resume))
      (halt)))

(define (start-threads)
  (let ((cc (current/cc)))
    (if cc
	(begin
	  (set! halt (lambda () (cc #f)))
	  (if (null? thread-queue)
	      (void)
	      (begin
		(let ((next-thread (car thread-queue)))
		  (set! thread-queue (cdr thread-queue))
		  (next-thread 'resume)))))
	(void))))

(define counter 10)

(define (make-thread-thunk name)
  (letrec ((loop (lambda ()
		   (if (< counter 0)
		       (quit))
		   (display "in thread ")
		   (display name)
		   (display "; counter = ")
		   (display counter)
		   (newline)
		   (set! counter (- counter 1))
		   (yield)
		   (loop))))
    loop))

(spawn (make-thread-thunk 'a))
(spawn (make-thread-thunk 'b))
(spawn (make-thread-thunk 'c))

(start-threads)
