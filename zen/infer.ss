(define (TInt) (tuple TInt))
(define (TBool) (tuple TBool))
(define (TUnit) (tuple TUnit))
(define (TFun x y) (tuple TFun x y))
(define (TVar i) (tuple TVar i))
(define (TTuple tag tuples) (tuple TTuple tag tuples))

(define (subst t0 v t)
  (case t0
    (TUnit (TUnit))
    (TInt (TInt))
    (TBool (TBool))
    (TFun
     (TFun (subst (field 0 t0) v t)
           (subst (field 1 t0) v t)))
    (TVar
     (if (eq? (field 0 t0) v) t t0))))

(define (update-type t subst)
  (case t
    (TUnit (TUnit))
    (TInt (TInt))
    (TBool (TBool))
    (TFun
     (TFun (update-type (field 0 t) subst)
           (update-type (field 1 t) subst)))
    (TVar
     (let ((v (assoc (field 0 t) subst)))
       (if v v t)))))

(define (extend-subst subst v t)
  (cons (cons v t)
        (map
         (lambda (item)
           (cons (car item)
                 (subst (cdr item) v t)))
         subst)))

(define (occur v t)
  (case t
    (TVar (eq? (field 0 t) v))
    (TFun
     (or (occur v (field 0 t))
         (occur v (field 1 t))))
    (TInt #f)
    (TBool #f)
    (TUnit #f)))

;; 'a M -> ('a -> 'b M) -> 'b M
(define (>>= aM a2bM)
  (case aM
    (None (None))
    (Some (a2bM (field 0 aM)))))

(define (Some x) (tuple Some x))
(define (None) (tuple None))

;; type -> type -> subst -> subst option
(define unifier
  (lambda (t1 t2)
    (lambda (s)
      (let ((t1 (update-type t1 s))
            (t2 (update-type t2 s)))
        (cond
         ((and (TInt? t1) (TInt? t2)) (Some s))
         ((and (TBool? t1) (TBool? t2)) (Some s))
         ((and (TUnit? t1) (TUnit? t2)) (Some s))
         ((and (TVar? t1) (TVar? t2)
               (eq? (field 0 t1) (field 0 t2)))
          (Some s))
         ((TVar? t1)
          (if (occur (field 0 t1) t2)
              (None)
              (Some (extend-subst s (field 0 t1) t2))))
         ((TVar? t2)
          (if (occur (field 1 t2) t1)
              (None)
              (Some (extend-subst s (field 0 t2) t1))))
         ((and (TFun? t1) (TFun? t2))
          (let ((a1 (field 0 t1))
                (e1 (field 1 t1))
                (a2 (field 0 t2))
                (e2 (field 1 t2)))
            (>>=
             (>>= (Some s) (unifier a1 a2))
             (unifier e1 e2)))))))))

(define (type-of exp env subst)
  (case exp
    (Bool (tuple _ (TBool) subst env))
    (Int (tuple _ (TInt) subst env))
    (Equal
     (let* ((ans1 (type-of (field 0 exp) env subst))
            (ta (field 0 ans1))
            (subst1 (field 1 ans1))
            (env (field 2 ans1)))
       (let ((subst2 (>>= subst1 (unifier ta (TInt)))))
         (let* ((ans2 (type-of (field 1 exp) (field 2 ans1) subst)))
           (let (()))))
