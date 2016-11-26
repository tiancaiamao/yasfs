(define-syntax tuple
  (syntax-rules ()
    [(_ tag v1 v2 ...) (vector 'tag v1 v2 ...)]
    [(_ tag) (vector 'tag)]))

(define (field n t) (vector-ref t (+ n 1)))

(define-syntax case
  (syntax-rules ()
    [(_ exp (tag e) ...)
     (cond
      ((eq? 'tag (vector-ref exp 0)) e)
      ...
      (else
       ;; if none matched, throw a exception
       (+ 23 "type infer failed")))]))
