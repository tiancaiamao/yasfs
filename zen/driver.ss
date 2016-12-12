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
  (let ((p
         (open-file-output-port "test.out"
                                (file-options no-fail)
                                (buffer-mode block))))
    (for-each (lambda (x) (emit-inst p x)) bc)
    (flush-output-port p)
    (close-output-port p)))

(define (eval-file file)
  (step-emit
   (step-compile
    (bruijn
     (parse
      (read-file file))))))
