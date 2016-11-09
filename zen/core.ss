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

;; (define (cons a b)
;;   (tuple Cons a b))
;; (define (car x)
;;   (case x
;;     (Cons (field 0 x))))
;; (define (cdr x)
;;   (case x
;;     (Cons (field 1 x))))
;; (define nil
;;   (tuple None))

;; (define map
;;   (lambda (f l)
;;     (case l
;;       (None nil)
;;       (Cons (cons (f (car l)) (map f (cdr l)))))))
