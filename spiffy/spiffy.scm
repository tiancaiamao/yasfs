(use intarweb)
(use tcp)
(use srfi-18)

(define (handler req resp)
  (display "hello world" (response-port resp))
  (finish-response-body resp))
  

(define (handle-incoming-request in out)
  (let ((req (read-request in))
	(resp (make-response
	       port: out
	       headers: (headers
			 `((content-type text/html))))))
    (handler req resp)
    (unless (port-closed? out) (flush-output out))))

(define (accept-loop listener)
  (let accept-next-connection ()
    (let-values (((in out) (tcp-accept listener)))
      (thread-start!
       (lambda ()
	 (let handle-next-request ()
	   (when (and (handle-incoming-request in out)
		      (not (port-closed? in))
		      (not (port-closed? out)))
		 (handle-next-request)))
	 (close-input-port in)
	 (close-output-port out))))
    (accept-next-connection)))

(define (start-server port)
  (let ((listener (tcp-listen port)))
    (accept-loop listener)))

		 
      
	 
