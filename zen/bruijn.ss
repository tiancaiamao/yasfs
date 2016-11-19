;; 如果找到返回int，找不到直接panic
(define (find-env env v)
  (define (find e v i)
    (if (null? e) (+ 3 "find-env找不到直接panic")
        (if (eq? (car e) v)
            i
            (find (cdr e) v (+ i 1)))))
  (find env v 0))

(define (extend-env env v)
  (if (null? v)
      env
      (extend-env (cons (car v) env) (cdr v))))

(define empty-env (lambda () '()))

(define name2tag
  (let ((count 0)
        (ht (make-hashtable symbol-hash eq?)))
    (lambda (name)
      (let ((find (symbol-hashtable-ref ht name -1)))
        (if (= find -1)
            (begin
              (set! count (+ count 1))
              (symbol-hashtable-set! ht name count)
              count)
            find)))))

(define (ast2lambda env ast)
  (case ast
    (Bool ast)
    (Int ast)
    (String ast)
    (Tuple
     (let ((tag (if (eq? (field 0 ast) '_)
                    0
                    (name2tag (field 0 ast))))
           (ls (map (lambda (x) (ast2lambda env x))
                    (field 1 ast))))
       (Tuple tag ls)))
    (Field
     (Field (field 0 ast) (ast2lambda env (field 1 ast))))
    (Fun
     (let ((args (field 0 ast))
           (ts (field 1 ast)))
       (let ((len (length args))
             (new-env (extend-env env args))
             (fn (lambda (e)
                   (lambda (v)
                     (ast2lambda e v)))))
         (Fun len (map (fn new-env) ts)))))
    (Fun1
     (let ((args (field 0 ast))
           (ts (field 1 ast)))
       (let ((len (length args))
             (new-env (extend-env env args))
             (fn (lambda (e)
                   (lambda (v)
                     (ast2lambda e v)))))
         (Fun1 len (map (fn new-env) ts)))))
    (Var (Var (find-env env (Var&s ast))))
    (Set (Set (find-env env (field 0 ast))
              (ast2lambda env (field 1 ast))))
    (Let
     (let ((arg-list (field 0 ast))
           (body (field 1 ast)))
       (Let (length arg-list)
            (map
             (lambda (x)
               (ast2lambda (extend-env env arg-list) x))
             body))))
    (App (App
          (ast2lambda env (App&t ast))
          (let ((fn (lambda (env)
                      (lambda (x)
                        (ast2lambda env x)))))
            (map (fn env) (App&ts ast)))))
    (Plus
     (Plus (ast2lambda env (field 0 ast))
           (ast2lambda env (field 1 ast))))
    (Sub
     (Sub (ast2lambda env (field 0 ast))
          (ast2lambda env (field 1 ast))))
    (Mul
     (Mul (ast2lambda env (field 0 ast))
          (ast2lambda env (field 1 ast))))
    (Div
     (Div (ast2lambda env (field 0 ast))
          (ast2lambda env (field 1 ast))))
    (If (If (ast2lambda env (field 0 ast))
            (ast2lambda env (field 1 ast))
            (ast2lambda env (field 2 ast))))
    (Equal
     (Equal (ast2lambda env (field 0 ast))
            (ast2lambda env (field 1 ast))))
    (Switch
     (let ((fn (lambda (x)
                 (cons (name2tag (car x))
                       (ast2lambda env (cdr x))))))
       (Switch (ast2lambda env (field 0 ast))
               (map fn (field 1 ast)))))
    ))
