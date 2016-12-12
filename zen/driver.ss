(load "core.ss")
(load "parser.ss")
(load "compiler.ss")
(load "emit.ss")

(define (read-file file)
  (define (loop p l)
    (let ((s (read p)))
      (if (eof-object? s)
          l
          (loop p (cons s l)))))
  (call-with-input-file file
    (lambda (port)
      (let ((input `(lambda ()
                      ,@(reverse
                         (loop port '())))))
        (caddr
         (macro2
          (macro1
           (macro0 input))))))))

(define (step-parse exp)
  (parse exp '()))

(define (step-compile ir)
  (compile ir (cons (tuple IStop) '()) 0))

(define (step-emit bc)
  (lambda (p)
    (for-each
     (lambda (x) (emit-inst p x))
     bc)))

(define (step-emit-file bc filename)
  (let ((p
         (open-file-output-port filename
                                (file-options no-fail)
                                (buffer-mode block))))
    ((step-emit bc) p)
    (flush-output-port p)
    (close-output-port p)))
