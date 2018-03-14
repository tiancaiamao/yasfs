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

(define (rewrite-fun1 input cache result)
  (pmatch input
          ()                 -> (reverse result)
          ('-> act . remain) -> (let ((tmp (list (reverse cache) act)))
                                  (rewrite-fun1 remain '() (cons tmp result)))
          (a . b)            -> (rewrite-fun1 b (cons a cache) result)))

(define (rewrite-fun input)
  (rewrite-fun1 input '() '()))

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

(fun extract-match-rules
     () -> '()
     (a '-> b . remain) -> (cons (list a b) (extract-match-rules remain)))

(fun ppat
     val (? number? x) kt kf -> `(if (&#equal? ,val ,x) ,kt ,kf)
     val (? symbol? pat) kt kf -> `(let ,pat ,val ,kt)
     val ('quote x) kt kf -> `(if (&#eq? ,val (quote ,x)) ,kt ,kf)
     val () kt kf -> `(if (null? ,val) ,kt ,kf)
     val (p1 . p2) kt kf ->
     (let ((v1 (gensym '&#v))
           (v2 (gensym '&#v)))
       `(if (pair? ,val)
            (let ,v1 (car ,val)
                 ,v2 (cdr ,val)
                 ,(ppat v1
                        p1
                        (ppat v2 p2 kt kf)
                        kf))
            ,kf)))

(fun rewrite-match
     _ () -> '(error "no match")
     val ((pat action) . remain) ->
     `(let $tmp ,val
        ,(ppat '$tmp pat action (rewrite-match val remain))))

(fun match-macro
     ('match val . rules) _ -> (rewrite-match val (extract-match-rules rules))
     x _ -> x)

(fun all-same-list1
     a () -> a
     a (a . b) -> (all-same-list1 a b)
     _ _ -> #f)

(fun all-same-list
     () -> #f
     (a . b) -> (all-same-list1 a b))

;; (all-same-list '(1 2 3 4 5))
;; (all-same-list '(1 1 1 1 1))

(fun gen-args n -> (string->symbol (format "&#a~a" n)))

(fun generate-args-list1
     0 ret -> ret
     n ret -> (generate-args-list1 (- n 1) (cons (gen-args n) ret)))

(fun generate-args-list
     n -> (generate-args-list1 n '()))

;; (generate-args-list 5)

(fun check-rules-arity
     rules -> (let* ((collected (map (lambda (x) (length (car x))) rules))
                     (arity (all-same-list collected)))
                (if (not arity)
                    (error "wrong pattern parameters")
                    arity)))

(fun insert->
     (x . y) -> (cons x (cons '-> y)))

(fun fun-rewrite rules -> (let ((rules1 (map insert-> rules)))
                            (foldl (lambda (ls x) (append x ls)) '() rules1)))

(fun fun-macro
     ('fun f . rules) -> (let* ((rules1 (rewrite-fun rules))
                                (arity (check-rules-arity rules1))
                                (args (generate-args-list arity))
                                (rules2 (fun-rewrite rules1)))
                           `(define ,f
                              (lambda ,args
                                ,(match-macro
                                 `(match (list ,@args)
                                       ,@rules2)))))
     x -> x)

;; (fun-macro '(fun map1
;;                  f ()      -> '()
;;                  f (a . b) -> (cons (f a) (map1 f b))))

(fun let-macro
     ('let x y v) -> `(&#let ,x ,y ,v)
     ('let x y z w . v) -> (list '&#let x y
                                 (let-macro
                                  `(let ,z ,w . ,v)))
     x -> x)

;; (let-macro
;;  '(let a 3 b 5 (+ a b)))

(fun &#let-macro
     ('&#let x y v) -> `((lambda (,x) ,v) ,y)
     x -> x)

;; (&#let-macro '(&#let a 3 (&#let b 5 (+ a b))))

(fun curry-macro
     ('lambda () v) -> `(/. _ ,v)
     ('lambda (x) v) -> `(/. ,x ,v)
     ('lambda (x . y) v) -> `(/. ,x
                               ,(curry-macro `(lambda ,y ,v)))
     x -> x)

;; (curry-macro '(lambda (a b) (+ a b)))

(fun list-macro
     ('list) -> '(quote ())
     ('list a . b) -> `(&#cons ,a ,(list-macro
                                  (cons 'list b))))

;; (list-macro
;;  '(list 1 2 3 4))

(fun compose
     () x -> x
     (f . fs) x -> (compose fs (f x)))

(fun walk
     f (x . y) -> (f (map f (cons x y)))
     f x -> (f x))

(define *macros* (list `(match . ,match-macro)
                       `(fun . ,fun-macro)
                       `(let . ,let-macro)
                       `(&#let . ,&#let-macro)
                       `(lambda . ,curry-macro)
                       `(list . ,list-macro)))

(fun macro?
     x -> (if (assq x *macros*)
              #t
              #f))

(fun get-macro
     m ((m . f) . _) -> f
     m (x . xs) -> (get-macro m xs))

(fun macroexpand1
     (? atom? x) -> x
     ('quote . x) -> (cons 'quote x)
     ('if c a b) -> (list 'if (macroexpand1 c) (macroexpand1 a) (macroexpand1 b))
     ('begin . l) -> (cons 'begin (map macroexpand1 l))
     ('set! a b) -> (list 'set! a (macroexpand1 b))
     ('define a b) -> (list 'define a (macroexpand1 b))
     ('/. x y) -> (list '/. x (macroexpand1 y))
     ((? macro? m) . x) -> ((get-macro m *macros*) (cons m x))
     ;; partial apply
     (f) -> `(,(macroexpand1 f) #f)
     (f a) -> `(,(macroexpand1 f) ,(macroexpand1 a))
     (f a . l) -> `((,f ,a) . ,l)
     x -> x)

(fun fix-point1
     f x x -> x
     f x y -> (fix-point1 f (f x) x))

(fun fix-point
     f x -> (fix-point1 f (f x) x))

(fun macroexpand
    x -> (fix-point macroexpand1 x))

(define-syntax /.
  (syntax-rules ()
    ((_ arg body)
     (lambda (arg)
       body))))

(define &#equal
  (lambda (x)
    (lambda (y)
      (equal? x y))))

(define &#eq?
  (lambda (x)
    (lambda (y)
      (eq? x y))))

(define repl
  (lambda ()
    (display "zenlisp#>	")
    (let* ((raw (read))
           (input (macroexpand raw))
           (result (eval input)))
      (display result)
      (newline)
      (repl))))

(macroexpand '(fun map1
                   f ()      -> '()
                   f (a . b) -> (&#cons (f a) (map1 f b))))

(macroexpand '(fun test
                   a b -> (cons a b)))

(define map1 (/. &#a1 (/. &#a2 ((/. $tmp (if (pair? $tmp) ((/. &#v99713 ((/. &#v99714 ((/. f (if (pair? &#v99714) ((/. &#v99715 ((/. &#v99716 (if (pair? &#v99715) ((/. &#v99717 ((/. &#v99718 ((/. a ((/. b (if (null? &#v99716) ((&#cons (f a)) ((map1 f) b)) ((/. $tmp (if (pair? $tmp) ((/. &#v99709 ((/. &#v99710 ((/. f (if (pair? &#v99710) ((/. &#v99711 ((/. &#v99712 (if (null? &#v99711) (if (null? &#v99712) (quote ()) (error "no match")) (error "no match"))) (cdr &#v99710))) (car &#v99710)) (error "no match"))) &#v99709)) (cdr $tmp))) (car $tmp)) (error "no match"))) ((&#cons &#a1) ((&#cons &#a2) (quote ())))))) &#v99718)) &#v99717)) (cdr &#v99715))) (car &#v99715)) ((/. $tmp (if (pair? $tmp) ((/. &#v99709 ((/. &#v99710 ((/. f (if (pair? &#v99710) ((/. &#v99711 ((/. &#v99712 (if (null? &#v99711) (if (null? &#v99712) (quote ()) (error "no match")) (error "no match"))) (cdr &#v99710))) (car &#v99710)) (error "no match"))) &#v99709)) (cdr $tmp))) (car $tmp)) (error "no match"))) ((&#cons &#a1) ((&#cons &#a2) (quote ())))))) (cdr &#v99714))) (car &#v99714)) ((/. $tmp (if (pair? $tmp) ((/. &#v99709 ((/. &#v99710 ((/. f (if (pair? &#v99710) ((/. &#v99711 ((/. &#v99712 (if (null? &#v99711) (if (null? &#v99712) (quote ()) (error "no match")) (error "no match"))) (cdr &#v99710))) (car &#v99710)) (error "no match"))) &#v99709)) (cdr $tmp))) (car $tmp)) (error "no match"))) ((&#cons &#a1) ((&#cons &#a2) (quote ())))))) &#v99713)) (cdr $tmp))) (car $tmp)) ((/. $tmp (if (pair? $tmp) ((/. &#v99709 ((/. &#v99710 ((/. f (if (pair? &#v99710) ((/. &#v99711 ((/. &#v99712 (if (null? &#v99711) (if (null? &#v99712) (quote ()) (error "no match")) (error "no match"))) (cdr &#v99710))) (car &#v99710)) (error "no match"))) &#v99709)) (cdr $tmp))) (car $tmp)) (error "no match"))) ((&#cons &#a1) ((&#cons &#a2) (quote ())))))) ((&#cons &#a1) ((&#cons &#a2) (quote ())))))))

((map1 (lambda (x) (+ x 1))) '(1 2 3))
