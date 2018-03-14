(use matchable)

(define-syntax fun
  (er-macro-transformer
   (lambda (input rename compare)
     (let* ((input (cdr input))
            (name (car input))
            (body (cdr input))
            (rules (split-body-to-rules body))
            (lam+ (compile-to-lambda+ name rules))
            (kl (compile-to-kl name lam+)))
       kl))))

(define compile-to-lambda+
  (lambda (name rules)
    (let* ((arity (arity-check name rules))
           (variables (parameters arity))
           (abstractions (map abstract-rule rules))
           (applications (map (lambda (x) (application-build variables x)) abstractions)))
      (cons variables applications))))

(define arity-check
  (lambda (name rules)
    (let* ((rule (car rules))
           (pattern (car rule)))
      (length pattern))))

(define iter
  (lambda (n f init)
    (if (= n 0)
        init
        (iter (- n 1) f (f init)))))

(define parameters
  (lambda (n)
    (define fn (lambda (l) (cons (gensym) l)))
    (iter n fn '())))

(define abstract-rule
  (lambda (rule)
    (abstraction-build (car rule) (cdr rule))))

;; (abstraction-build '(n '()) 'n) => (/. n (/. '() n))
(define abstraction-build
  (lambda (pattern action)
    (if (null? pattern)
        action
        `(/. ,(car pattern) ,(abstraction-build (cdr pattern) action)))))

;; (application-build '(g1 g2) '(/. n (/. '() n))) => (((/. n (/. '() n)) g1) g2)
(define application-build
  (lambda (vars abstraction)
    (if (null? vars)
        abstraction
        (application-build (cdr vars) `(,abstraction ,(car vars))))))

(define list-split
  (lambda (key l)
    (define (list-split-help key left right)
      (cond
       ((null? right) (values (reverse left) right))
       ((eq? (car right) key) (values (reverse left) right))
       (else (list-split-help key (cons (car right) left) (cdr right)))))
    (list-split-help key '() l)))

(define split-body-to-rules
  (lambda (body)

    (define decode
      (lambda (x)
        (cond
         ((null? x) '(:nil))
         ((not (pair? x)) x)
         ((udf? (car x)) x)
         (else
          `(:cons ,(car x) ,(decode (cdr x)))))))

    (define (split-one-rule l)
      (let-values (((pattern1 rem) (list-split '-> l)))
        (let ((action (cadr rem))
              (rem1 (cddr rem))
              (pattern (map decode pattern1)))
          (values (cons pattern action) rem1))))

    (define (split-helper rules body)
      (let-values (((rule remain) (split-one-rule body)))
        (if (null? remain)
            (reverse (cons rule rules))
            (split-helper (cons rule rules) remain))))

    (split-helper '() body)))

(define compile-to-kl
  (lambda (name lam+)
    (let* ((variables (car lam+))
           (applications (cdr lam+))
           (arity (length variables))
           (reduced (map reduce applications))
           (condexpr (cond-expression name variables reduced)))
      `(define ,name
         (lambda ,variables ,condexpr)))))

(define cond-expression
  (lambda (name variable reduced)
    (define (fn x)
      (if (eq? (car x) 'where)
          (cdr x)
          `(#t ,x)))
    (let ((body (map fn reduced)))
      (cons 'cond body))))

(define occurrences
  (lambda (x y)
    (cond
     ((null? y) #f)
     ((pair? y) (occurrences x (car y)) (occurrences x (cdr y)))
     ((symbol? y) (eq? x y))
     (else #f))))

;; z[y/x]
(define beta
  (lambda (x y z)
    (match z
           [(? (lambda (x) (eq? x y))) x]
           [('/. (? (lambda (C) (occurrences y C))) D) z]
           [(C . D) (cons (beta x y C) (beta x y D))]
           [_ z])))

(define reduce-rule
  (lambda (application)
    (match application
           [(('/. (C ARGS ...) Z) W)
            (let ((C? (symbol-append C '?))
                  (C@ (symbol-append C '@)))
              `(where (,C? ,W)
                      ,(curry-partial ARGS C@ W Z 0)))]
           [(('/. (? symbol? X) Y) Z) (beta Z X Y)]
           [`((/. ,C ,X) ,Y) `(where (equal? ,C ,Y) ,X)]
           [_ application])))

(define curry-partial
  (lambda (ARGS C@ W Z n)
    (match ARGS
           [() Z]
           [(a . b) `((/. ,a ,(curry-partial b C@ W Z (+ n 1))) (,C@ ,n ,W))])))

(define reduce-once
  (lambda (application)
    (match application
           [`((/. (cons ,X ,Y) ,Z) ,W) (reduce-rule application)]
           [`((/. ,(? symbol? X) ,Y) ,Z) (reduce-rule application)]
           [`((/. ,C ,X) ,Y) (reduce-rule application)]
           [`(where ,P ,Q) `(where ,P ,(reduce-rule Q))]
           [`(,X ,Y) `(,(reduce-rule X) ,Y)]
           [_ application])))

(define fixpoint
  (lambda (f x)
    (fix-point-help f x (f x))))

(define fix-point-help
  (lambda (f x y)
    (if (equal? x y)
        x
        (fix-point-help f y (f y)))))

(define reduce
  (lambda (application)
    (fixpoint reduce-once application)))

(reduce-once '(((/. V (/. (:nil) #f)) A) B))
(reduce-once '((/. (:nil) #f) B))
(reduce-once '((/. (:nil) #f) B))
(reduce-once '(where (:nil? B) #f))

;; (reduce-once '(((/. V (/. '() #f)) A) B))
;; (reduce-once '((/. '() #f) B))
;; (reduce-once '(where (equal? '() B) X))


(reduce-once '(((/. X (/. (:cons U W) (where (= U X) #t))) A) B))
(reduce-once '((/. (:cons U W) (where (= U A) #t)) B))
(reduce-once '(where (:cons? B) ((/. U ((/. W (where (= U A) #t)) (:cons@ 1 B))) (:cons@ 0 B))))
(reduce-once '(where (:cons? B) ((/. W (where (= (:cons@ 0 B) A) #t)) (:cons@ 1 B))))
(reduce-once '(where (:cons? B) (where (= (:cons@ 0 B) A) #t)))

;; (reduce-once '(((/. X (/. (cons U W) (where (= U X) #t))) A) B))
;; (reduce-once '((/. (cons U W) (where (= U A) #t)) B))
;; (reduce-once '(where
;;                (cons? B)
;;                ((/. U ((/. W (where (= U A) #t)) (cdr B))) (car B))))
;; (reduce-once '(where (cons? B) ((/. W (where (= (car B) A) #t)) (cdr B))))
;; (reduce-once '(where (cons? B) (where (= (car B) A) #t)))

(reduce-once '(((/. X (/. (:cons Z Y) ((M X) Y))) A) B))
(reduce-once '((/. (:cons Z Y) ((M A) Y)) B))
(reduce-once '(where (:cons? B) ((/. Z ((/. Y ((M A) Y)) (:cons@ 1 B))) (:cons@ 0 B))))
(reduce-once '(where (:cons? B) ((/. Y ((M A) Y)) (:cons@ 1 B))))
(reduce-once '(where (:cons? B) ((M A) (:cons@ 1 B))))

;; (reduce-once '(((/. X (/. (cons Z Y) ((M X) Y))) A) B))
;; (reduce-once '((/. (cons Z Y) ((M A) Y)) B))
;; (reduce-once '(where
;;                (cons? B)
;;                ((/. Z ((/. Y ((M A) Y)) (cdr B))) (car B))))
;; (reduce-once '(where (cons? B) ((/. Y ((M A) Y)) (cdr B))))
;; (reduce-once '(where (cons? B) ((M A) (cdr B))))

(expand '(fun x
     (:some a) -> a
     (:none) -> 5))

;; (type list (a) (:cons a (list a)) (:nil))

(expand '(type option (a) (var (:some a) (:none))))
(##core#begin
 (begin
   (define-record :some a)
   (define :some@
     (lambda (n s)
       (case n
         ((0) (:some-a s))))))
 (define-record :none))

(type option (a) (var (:some a) (:none)))

(define (generate-at base l)
  `(define ,(symbol-append base '@)
     (lambda (n s)
       (case n
         ,@(generate 0 base l)))))

(define (generate n base l)
  (if (null? l)
      '()
      (let* ((fname (symbol-append base '- (car l)))
             (item `((,n) (,fname s))))
        (cons item (generate (+ n 1) base (cdr l))))))

(define-syntax type
  (er-macro-transformer
   (lambda (input rename compare)

     (define (type-define-record label)
       (if (null? (cdr label))
           (cons 'define-record label)
           `(begin
              ,(cons 'define-record label)
              ,(generate-at (car label) (cdr label)))))

     (let* ((input (cadddr input))
            (labels (cdr input)))
       `(begin
          ,@(map type-define-record labels))))))

(define some3 (make-:some 3))
(:some? some3)
(:some@ 0 some3)

(define :cons? pair?)
(define :cons@
  (lambda (n v)
    (case n
      ((0) (car v))
      ((1) (cdr v)))))
(define :nil? null?)

(expand '(fun len
     () -> 0
     (a . b) -> (+ 1 (len b))))

(expand '(fun fact
     0 -> 1
     n -> (* n (fact (- n 1)))))

