(define (IStackAccess n) (tuple IStackAccess n))
(define (IEnvAccess n) (tuple IEnvAccess n))
(define (IReturn) (tuple IReturn))
(define (IPush) (tuple IPush))
(define (IPlus) (tuple IPlus))
(define (ISub) (tuple ISub))
(define (IMul) (tuple IMul))
(define (IDiv) (tuple IDiv))
(define (IEqual) (tuple IEqual))
(define (IStop) (tuple IStop))
(define (IGrab n) (tuple IGrab n))
(define (IApply) (tuple IApply))
(define (IClosure code) (tuple IClosure code))
(define (IPushRetAddr code) (tuple IPushRetAddr code))
(define (IConst v) (tuple IConst v))
(define (IBool v) (tuple IBool v))
(define (IString v) (tuple IString v))
(define (IBranch then else) (tuple IBranch then else))

(define (compile exp code threshold)
  (case exp
    (Int (cons (IConst (field 0 exp)) code))
    (Bool (cons (IBool (field 0 exp)) code))
    (String (cons (IString (field 0 exp)) code))
    (Var
     (let ((n (Var&s exp)))
       (if (< n threshold)
           (cons (IStackAccess n) code)
           (cons (IEnvAccess (- n threshold)) code))))
    (Fun
     (let ((n (field 0 exp))
           (ts (field 1 exp)))
       (cons (IClosure (compile-tail exp n)) code)))
    (App
     (cons (IPushRetAddr code) (compile-tail exp threshold)))
    (Plus
     (let ((a (field 0 exp))
           (b (field 1 exp)))
       (compile
        a
        (cons (IPush)
              (compile
               b
               (cons (IPlus) code)
               threshold))
        threshold)))
    (Sub
     (let ((a (field 0 exp))
           (b (field 1 exp)))
       (compile
        a
        (cons (IPush)
              (compile
               b
               (cons (ISub) code)
               threshold))
        threshold)))
    (Mul
     (let ((a (field 0 exp))
           (b (field 1 exp)))
       (compile
        a
        (cons (IPush)
              (compile
               b
               (cons (IMul) code)
               threshold))
        threshold)))
    (Div
     (let ((a (field 0 exp))
           (b (field 1 exp)))
       (compile
        a
        (cons (IPush)
              (compile
               b
               (cons (IDiv) code)
               threshold))
        threshold)))
    (Equal
     (compile (field 0 exp)
              (cons (IPush)
                    (compile (field 1 exp)
                             (cons (IEqual) code)
                             threshold))
              threshold))
    (If
     (compile (field 0 exp)
              (cons (IBranch (compile (field 1 exp) code threshold)
                       (compile (field 2 exp) code threshold)) '())
              threshold))
    ))

(define (compile-tail exp threshold)
  (case exp
    (Int (compile exp (cons (IReturn) '()) threshold))
    (Bool (compile exp (cons (IReturn) '()) threshold))
    (String (compile exp (cons (IReturn) '()) threshold))
    (Var (compile exp (cons (IReturn) '()) threshold))
    (Plus (compile exp (cons (IReturn) '()) threshold))
    (Sub (compile exp (cons (IReturn) '()) threshold))
    (Mul (compile exp (cons (IReturn) '()) threshold))
    (Div (compile exp (cons (IReturn) '()) threshold))
    (Equal (compile exp (cons (IReturn) '()) threshold))
    (Fun (let ((n (field 0 exp))
               (ts (field 1 exp)))
           (cons (IGrab n) (compile-body ts n))))
    (App (let ((t (field 0 exp))
               (ts (field 1 exp)))
           (let ((init (compile t (cons (IApply) '()) threshold))
                 (f (lambda (a b)
                      (compile b (cons (IPush) a) threshold))))
             (fold-left f init ts))))
    ))

(define (compile-body ts threshold)
  (if (null? ts) '()
      (if (null? (cdr ts))
          (compile (car ts) (cons (IReturn) '()) threshold)
          (compile (car ts)
                   (compile-body (cdr ts) threshold) threshold))))
