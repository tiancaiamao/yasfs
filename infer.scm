(use matchable)

;; int
;; bool
;; (type -> type)
;; 3
;; (var (apple : type) (orange))
;; (var ())

;; subst all occur v in t1 to t2  -- t1[v/t2]
(define (subst type1 tvar type2)
  (match type1
         ['int 'int]
         ['bool 'bool]
         [(a '-> b) `(,(subst a tvar type2) -> ,(subst b tvar type2))]
         [('var l ...) `(var ,(subst-label-list l tvar type2))]
         [('vec l ...) `(var ,(subst-label-list l tvar type2))]
         [var (if (= tvar var) type2 var)]))

(define (subst-label-list l tvar type)
  (map-label-list (lambda (type1) (subst type1 tvar type)) l))

(define (map-label-list fn l)
  (define (replace-one x)
    (match x
           [(label) x]
           [(label . type1) `(,label . ,(fn type1))]))
  (map replace-one l))

;; (subst 'int '1 'int)
;; (subst '(1 -> bool) '1 'int)
;; (subst '3 '4 'int)
;; (subst '(var (apple : 3) (orange)) 3 '(int -> bool))

(define (apply-subst t subst)
  (match t
         ['int 'int]
         ['bool 'bool]
         [(a '-> b) `(,(apply-subst a subst) -> ,(apply-subst b subst))]
         [('var l ...) `(var ,@(map-label-list (lambda (x) (apply-subst x subst)) l))]
         [('vec l ...) `(vec ,@(map-label-list (lambda (x) (apply-subst x subst)) l))]
         [var (let ((find (assoc var subst)))
                (if find (cdr find) var))]))

(apply-subst '(4 -> (var (apple . 2))) '((1 . int) (2 . bool) (3 . (int -> int))))
(apply-subst '(var (:apple 2)) '())
(apply-subst '(var (:orange 1) (:nil)) '())

(define empty-subst (lambda () '()))
(define (extend-subst subst tvar type) (cons (cons tvar type) subst))

(define (for-any fn l)
  (if (null? l)
      #f
      (or (fn (car l)) (for-any fn (cdr l)))))

(define (filter fn l)
  (match l
         ['() '()]
         [(a . b) (if (fn a)
                      (cons a (filter fn b))
                      (filter fn b))]))

(define (init-tenv) '((* . (int -> (int -> int)))
                      (+ . (int -> (int -> int)))
                      (> . (int -> (int -> bool)))))


(define (extend-tenv tvar type tenv)
  (cons (cons tvar type) tenv))

(define occur?
  (lambda (tvar type)
    (define (not-null? x) (not (null? x)))
    (match type
           ['int #f]
           ['bool #f]
           [(a '-> b) (or (occur? tvar a) (occur? tvar b))]
           [('var l ...) (for-any (lambda (x) (occur? tvar x))
                                  (foldl (lambda (res x) (append (cdr x) res)) '() l))]
           [('vec l ...) (for-any (lambda (x) (occur? tvar x))
                                  (foldl (lambda (res x) (append (cdr x) res)) '() l))]
           [(? udf-type?)
            (for-any (lambda (x) (occur? tvar x)) (cdr type))]
           [_ (= tvar type)])))

;; (occur? 3 '(int -> int))
;; (occur? 3 '(bool -> 3))
;; (occur? 3 '(var (:name (bool -> 4)) (:orange) (:apple 5)))
;; (occur? 3 '(var (:name (bool -> 4)) (:orange) (:apple 3)))
(occur? 3 '(var (:cons 1 (list 1)) (:nil)))

(define report-error
  (lambda (a b)
    (error (sprintf "not match ~% ~%" a b))))

(define tvar? integer?)
(define tvar
  (let ((idx 0))
    (lambda ()
      (set! idx (+ idx 1))
      idx)))

(define unify
  (lambda (t1 t2 subst)
    (let ((t1 (apply-subst t1 subst))
          (t2 (apply-subst t2 subst)))
      (cond
       ((equal? t1 t2) subst)
       ((and (tvar? t1) (not (occur? t1 t2)))
        (extend-subst subst t1 t2))
       ((and (tvar? t2) (not (occur? t2 t1)))
        (extend-subst subst t2 t1))
       ((and (udf-type? t1) (udf-type? t2) (eq? (car t1) (car t2)))
        (unify-list (cdr t1) (cdr t2) subst))
       (else
        (match (cons t1 t2)
               [((a '-> b) . (c '-> d))
                (let* ((subst1 (unify a c subst))
                       (subst2 (unify b d subst1)))
                  subst2)]
               [(('var l1 ...) . ('var l2 ...))
                (let* ((fn (lambda (subst p) (unify-var (car p) (cdr p) subst)))
                       (vars (map cons l1 l2)))
                  (foldl fn subst vars))]
               ;; [((ver l1 ...) . (ver l1 ...))]
               [_ (report-error t1 t2)])
        )))))

;; (unify 'int 'bool (empty-subst))
;; (unify 'int 'int (empty-subst))
;; (unify 'int '3 (empty-subst))
;; (unify 3 3 '())
;; (unify '(3 -> bool) '(int -> bool) '())
;; (unify '(3 -> bool) '(int -> 4) '())
;; (unify '(var (:orange 1)) '(var (:orange int)) '())
;; (unify '(var (:cons 3 (list int)) (:nil)) '(var (:cons int (list 5)) (:nil)) '())

(define (udf-type? x)
  (and (pair? x)
       (not (eq? (car x) 'var))
       (not (eq? (car x) 'rec))
       (assoc (car x) type-table)))

(define unify-list
  (lambda (l1 l2 subst)
    (cond
     [(and (null? l1) (null? l2)) subst]
     [(null? l1) (report-error l1 l2)]
     [(null? l2) (report-error l1 l2)]
     [else
      (let ((subst1 (unify (car l1) (car l2) subst)))
        (unify-list (cdr l1) (cdr l2) subst1))])))
;; (unify-list '(list 3) '(list int) '())
;; (unify '(list int) '(list 3) '())


(define unify-var
  (lambda (p1 p2 subst)
    (if (not (eq? (car p1) (car p2)))
        (report-error p1 p2)
        (unify-list (cdr p1) (cdr p2) subst))))
;; (unify-var '(:cons 3 (list int)) '(:cons bool (list 4)) '())
;; (unify-var '(:cons int (list int)) '(:cons 3 (list 3)) '())
;; (unify-var '(:orange 1) '(:orange int) '())
(unify-var '(:orange 1) '(:orange int) '())

(define type-of
  (lambda (exp env subst)
    (match exp
           [(? integer?) (values 'int subst)]
           [(? boolean?) (values 'bool subst)]
           [(? symbol?)
            (let ((find (assoc exp env)))
              (if find
                  (values (cdr find) subst)
                  (values (tvar) subst)))]
           [('if e1 e2 e3)
            (let-values (((t1 subst1) (type-of e1 env subst)))
              (let ((subst2 (unify 'bool t1 subst1)))
                (let-values (((t2 subst3) (type-of e2 env subst2)))
                  (let-values (((t3 subst4) (type-of e3 env subst3)))
                    (values t2 (unify t2 t3 subst4))))))]
           [('case x sels ...)
            (type-of-case-expr x sels env subst)]
           [('/. x e)
            (let ((tx (tvar)))
              (let-values (((ty subst1) (type-of e (extend-tenv x tx env ) subst)))
                (values `(,tx -> ,ty) subst1)))]
           [(e1 e2)
            (let-values (((t1 subst1) (type-of e1 env subst)))
              (let-values (((t2 subst2) (type-of e2 env subst1)))
                (match t1
                       [(a '-> b) (values b (unify a t2 subst2))])))])))

(define type-table '())
(define constructor-table '())
(define (add-type def)
  (match def
         [('type name args ('var label ...))
          (begin
            (set! constructor-table (append (map (lambda (x) (cons (car x) name)) label) constructor-table))
            (set! type-table (cons (cdr def) type-table)))]))

(add-type '(type list (a) (var (:cons a (list a)) (:nil))))
(add-type '(type fruit () (var (:apple int) (:orange))))

(define (gen-case-type patterns)
  (define (collect-constructors l) (map car l))
  (define (get-type-by-constructors cs)
    ;; TODO check
    (let ((type (cdr (assoc (car cs) constructor-table))))
      (assoc type type-table)))
  (define (replace from to body)
    (if (null? body)
        '()
        (let ((hd (car body))
              (tl (cdr body)))
          (cond
           ((and (symbol? hd) (eq? hd from))    (cons to (replace from to tl)))
           ((pair? hd) (cons (replace from to hd) (replace from to tl)))
           (else (cons hd (replace from to tl)))))))
  (define (instance type tv)
    (match type
           [(name (param) body)
            (replace param tv body)]))

  (let* ((constructors (collect-constructors patterns))
         (type (get-type-by-constructors constructors))
         (tv (tvar))
         (inst (instance type tv)))
    inst))

(define (extend-tenv-use-inst env inst patterns)
  (define (match-one inst pattern)
    (let* ((label (car pattern))
           (rest (cdr pattern))
           (find (assoc label (cdr inst))))
      (map cons rest (cdr find))))
  (define (fn env pattern) (append (match-one inst pattern) env))
  (foldl fn env patterns))

(extend-tenv-use-inst '() '(val (:cons 1 (list 1)) (:nil)) '((:cons a b) (:nil)))

(define (type-of-case-expr e0 rules env subst)
  (let* ((patterns (map car rules))
         (es (map cadr rules))
         (t1 (gen-case-type patterns))
         (env1 (extend-tenv-use-inst env t1 patterns)))
    (let-values (((t0 subst) (type-of e0 env subst)))
      (let ((subst1 (unify t0 t1 subst)))
        (let-values (((t0 subst2) (type-of (car es) env1 subst)))
          (type-of-expr-list t0 (cdr es) env1 subst2))))))

(type-of-case-expr 'x '(((:cons ((+ a) 1) b) a) ((:nil) 2)) (init-tenv) '())


(define (type-of-expr-list t0 es env subst)
  (if (null? es)
      (values t0 subst)
      (let-values (((t1 subst1) (type-of (car es) env subst)))
        (let ((subst2 (unify t0 t1 subst1)))
          (type-of-expr-list t0 (cdr es) env subst2)))))

;; (type-of-expr-list 'bool '(x y) '() '())
;; (type-of-expr-list 'int '(x (if #t 3 y)) '() '())
;; (type-of-expr-list 'int '(1 2) '() '())

;; (type-of '(if #t 1 #f) (init-tenv) '())
;; (type-of 1 '() '())
;; (type-of '(/. T (if #t (/. X X) T)) '() '())
;; (type-of #t '() '())
;; (type-of 'bbc (init-tenv) '())
;; (type-of '(/. x 1) '() '())
;; (type-of '(/. X X) '() '())
;; (type-of '((/. X (/. Y X)) 1) '() '())
;; (type-of '((/. X (/. Y Y)) 1) '() '())
;; (type-of '(/. X (/. Y (if X 1 2))) (init-tenv) '())
;; (type-of '+ (init-tenv) '())
;; (type-of '((+ 1) 2) (init-tenv) '())
;; (type-of '(+ 1) (init-tenv) '())
;; (type-of '((+ 1) x) (init-tenv) '())
;; (type-of '(case x
;;             [(:cons a b) ((+ a) 1)]
;;             [(:nil) 2]) (init-tenv) '())

(define infer
  (lambda (exp)
    (let-values (((type subst) (type-of exp (init-tenv) '())))
      (apply-subst type subst))))

(infer '(case x
            [(:cons a b) ((+ a) 1)]
            [(:nil) 2]))


(define gen-var
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      id)))

(define generate-constraints
  (lambda (exp env cstr-add debug)
    (let* ((tvar (gen-var)))
      (match exp
             [(? integer?)
              (cstr-add (cons tvar 'int))]
             [(? boolean?)
              (cstr-add (cons tvar 'bool))]
             [(? symbol?)
              (let ((find (assq exp env)))
                (if find
                    (set! tvar (cdr find))))]
             [('if e1 e2 e3)
              (let* ((t1 (generate-constraints e1 env cstr-add debug))
                     (t2 (generate-constraints e2 env cstr-add debug))
                     (t3 (generate-constraints e3 env cstr-add debug)))
                (cstr-add (cons t1 'bool))
                (cstr-add (cons t2 t3))
                (cstr-add (cons tvar t2)))]
             [('case x sels ...)
              (generate-constraints-case-expr x sels env cstr-add debug)]
             [('/. x e)
              (let* ((tx (gen-var))
                     (env1 (cons (cons x tx) env))
                     (te (generate-constraints e env1 cstr-add debug)))
                (cstr-add (cons tvar `(,tx -> ,te))))]
             [(e1 e2)
              (let* ((t1 (generate-constraints e1 env cstr-add debug))
                     (t2 (generate-constraints e2 env cstr-add debug)))
                (cstr-add (cons t1 `(,t2 -> ,tvar)))
                (let ((find (builtin e1)))
                  (if find
                      (cstr-add (cons t1 (cdr find))))))])
      (debug (cons tvar exp))
      tvar)))

(define generate-constraints-case-expr
  (lambda (e0 sels env cstr-add debug)
    (let* ((t0 (generate-constraints e0 env cstr-add debug))
           (tags (map caar sels)))
      (cstr-add (cons t0 `(var ,tags)))
      (for-each ))))

;; (define generate-constraints-case-expr-sel
;;   (lambda (sel env cstr-add debug)
;;     (let* ((tx (gen-var))))
;;     (generate-constraints e env1 cstr-add debug)
;;     (cstr-add )))

(define (builtin x)
  (and (symbol? x)
       (assq x '((* . (int -> (int -> int)))
                 (+ . (int -> (int -> int)))
                 (> . (int -> (int -> bool)))))))

(define (generate-constraints1 exp)
  (let* ((cstr '())
         (cstr-add (lambda (x) (set! cstr (cons x cstr)))))
    (values (generate-constraints exp '() cstr-add display) cstr)))
