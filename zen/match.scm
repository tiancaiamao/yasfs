(use matchable)

'(match x
   3 -> 3
   'a -> 'a
   a -> a
   (p1 . p2) -> p1)

'(match x
   (3 3)
   ('a 'a)
   (a a)
   ((p1 . p2) p1))

(define (extract-rules input)
  (extract-rules1 (cddr input) (car input) '() '()))

(define (extract-rules1 input pattern cache result)
  (match input
    ['() (reverse (cons (cons pattern (reverse cache)) result))]
    [`(,x -> . ,remain)     (let ((v (cons pattern (reverse cache))))
                              (extract-rules1 remain x '() (cons v result)))]
    [`(,x . ,y)   (extract-rules1 y pattern (cons x cache) result)]))

;; (extract-rules '(3 -> 3 'a -> a (p1 . p2) -> p2))
;; ((3 3) ('a a) ((p1 . p2) p2))

(define (rewrite input)
  (let ((val (cadr input))
        (raw (cddr input)))
    (rewrite-match val (extract-rules raw))))

(define (rewrite-match val rules)
  (match rules
    ['() '(error "no match")]
    [`((,pat ,action) . ,remain)
     (ppat val pat action (rewrite-match val remain))]))

(define (ppat val pat kt kf)
  (match pat
    [(? number? x) `(if (equal? ,val ,x) ,kt ,kf)]
    [`(quote ,x) `(if (eq? ,val ,pat) ,kt ,kf)]
    ['() `(if (null? ,val) ,kt ,kf)]
    [(? symbol?) `(let ((,pat ,val))
                    ,kt)]
    [`(,p1 . ,p2)
     `(if (pair? ,val)
          (let ((v1 (car ,val))
                (v2 (cdr ,val)))
            ,(ppat 'v1
                   p1
                   (ppat 'v2 p2 kt kf)
                   kf))
          ,kf)]))

(rewrite-match 'x '((3 3)))
(rewrite-match 'x '(('a 3)))

;; (rewrite '(match x
;;             (1 . 2) -> 42
;;             3 -> 3
;;             'a -> 'a
;;             $v -> $v
;;             ))

;; (let ((x  (cons 1 2)))
;;   (if (pair? x) (let ((v1 (car x)) (v2 (cdr x))) (if (equal? v1 1) (if (equal? v2 2) 42 (if (equal? x 3) 3 (if (eq? x 'a) 'a (let (($v x)) $v)))) (if (equal? x 3) 3 (if (eq? x 'a) 'a (let (($v x)) $v))))) (if (equal? x 3) 3 (if (eq? x 'a) 'a (let (($v x)) $v))))
;;   )

(rewrite '(match x(1 2 3) -> 42))

(rewrite-match 'x '(((1 2 3) 42)))
(let ((x '(2 3)))
  (if (pair? x)
      (let ((v1 (car x)) (v2 (cdr x)))
        (if (equal? v1 1)
            (if (pair? v2)
                (let ((v1 (car v2)) (v2 (cdr v2)))
                  (if (equal? v1 2)
                      (if (pair? v2)
                          (let ((v1 (car v2)) (v2 (cdr v2)))
                            (if (equal? v1 3)
                                (if (null? v2)
                                    42
                                    (error "no match"))
                                (error "no match")))
                          (error "no match"))
                      (error "no match")))
                (error "no match"))
            (error "no match")))
      (error "no match")))

(define-syntax pmatch
  (er-macro-transformer
   (lambda (input rename compare)
     (rewrite input))))



(macroexpand
 '(pmatch 1
          1 -> 42))

(define input '(pmatch 34
                       a -> (+ a 1)))

(extract-rules (cddr input))
`(match ,(cadr input) ,@(extract-rules (cddr input)))

(match 34
       ((a (+ a 1))))

(pmatch '()
        1 -> 24
        ('a 'b 'c) -> 444
        () -> #t
        (a . b) -> (+ a 1))

(func map
      / f () -> 123
      / f (a . b) -> (cons (f a) (map f b)))

(define (rewrite input)
  (let ((val (cadr input))
        (raw (cddr input)))
    (rewrite-match val (extract-rules raw))))

;; (define-syntax pmatch
;;   (er-macro-transformer
;;    (lambda (input rename compare)
;;      `(match ,(cadr input) ,@(extract-rules (cddr input))))))
