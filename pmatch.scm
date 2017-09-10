(use matchable)

(define-syntax pmatch
  (er-macro-transformer
   (lambda (input rename compare)
     ;; input (pat1 -> act1 pat2 -> act2 ...)
     ;; output ((pat1 act1) (pat2 act2) ...)
     (define (extract-rules input)
       (if (null? input)
           input
           (cons (list (car input) (caddr input))
                 (extract-rules (cdddr input)))))

     (let* ((raw (cddr input))
            (rules (extract-rules raw)))
       `(match ,(cadr input) ,@rules)))))

(define (rewrite-fun input)
  (rewrite-fun1 input '() '()))

(define (rewrite-fun1 input cache result)
  (pmatch input
          ()                 -> (reverse result)
          ('-> act . remain) -> (let ((tmp (list (reverse cache) act)))
                                  (rewrite-fun1 remain '() (cons tmp result)))
          (a . b)            -> (rewrite-fun1 b (cons a cache) result)))

(define-syntax fun
  (er-macro-transformer
   (lambda (input rename compare)
     (let* ((raw (cddr input))
            (rules (rewrite-fun raw)))
       `(define ,(cadr input)
          (match-lambda*
           ,@rules)))
     )))

;; (fun map
;;      f ()      -> '()
;;      f (a . b) -> (cons (f a) (map f b)))


