(define-syntax tuple
  (syntax-rules ()
    ((_ tag v ...)
     (vector 'tag v ...))))

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

(define-syntax match-tuple-one
  (syntax-rules ()
    ((_ x (tag) body)
     body)
    ((_ x (tag v) body)
     (let ((v (vector-ref x 1)))
       body))
    ((_ x (tag v1 v2) body)
     (let ((v1 (vector-ref x 1))
           (v2 (vector-ref x 2)))
       body))
    ((_ x (tag v1 v2 v3) body)
     (let ((v1 (vector-ref x 1))
           (v2 (vector-ref x 2))
           (v3 (vector-ref x 3)))
       body))))

(define-syntax match-tuple
  (syntax-rules ()
    ((_ x ((tag v ...) body) ...)
     (let ((tmp x))
       (cond
        ((eq? (quote tag) (vector-ref x 0))
         (match-tuple-one tmp (tag v ...) body))
        ...)))))
