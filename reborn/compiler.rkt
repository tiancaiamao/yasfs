(define atom? 
  (lambda (v)
    (not (pair? v))))
;;; Environment is held by a global variable. 
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.

(define *env* '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in R)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . index)
;;; or predefined (and immutable) then return
;;;                 (PREDEFINED . index)

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n) ) )

(define (local-variable? r i n)
  (and (pair? r)
       (let scan ((names (car r))
                  (j 0) )
         (cond ((pair? names) 
                (if (eq? n (car names))
                    `(local ,i . ,j)
                    (scan (cdr names) (+ 1 j)) ) )
               ((null? names)
                (local-variable? (cdr r) (+ i 1) n) )
               ((eq? n names) `(local ,i . ,j)) ) ) ) )


(define (global-variable? g n)
  (let ((var (assq n g)))
    (and (pair? var)
         (cdr var) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of local environments, they contain the values of
;;; the local variables (but global and predefined variables).
;;; Runtime environment or, activation frames, are represented by 
;;; vectors (named v*). They have the following structure:
;;;           +------------+
;;;           | next       |  ---> next V*
;;;           | argument 0 |  value of the first argument
;;;           | argument 1 |  value of the second argument
;;;           .            .
;;;           | free slot  |  Free slot for nary variable
;;;           +------------+
;;; The number of arguments can be extracted from the size of the
;;; activation frame.
#|
(define-class environment Object 
  ( next ) )

(define-class activation-frame environment
  ( (* argument) ) )

(define (sr-extend* sr v*)
  (set-environment-next! v* sr)
  v* )
|#

;; TODO
(define activation-frame-argument-length 
  (lambda (v)
    1))

(define sr.init '())

;;; Fetch the value of the Ith argument of the Jth frame.
#|
(define (deep-fetch sr i j)
  (if (= i 0)
      (activation-frame-argument sr j)
      (deep-fetch (environment-next sr) (- i 1) j) ) )

(define (deep-update! sr i j v)
  (if (= i 0)
      (set-activation-frame-argument! sr j v)
      (deep-update! (environment-next sr) (- i 1) j v) ) )
|#
;;; R is the static representation of the runtime local environment.
;;; It is represented by a list of list of variables (the classical
;;; rib cage). 

(define (r-extend* r n*)
  (cons n* r) )

(define r.init '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.
;;; It actually tolerates only 100 new global variables.

;;; G.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the SG.CURRENT vector.

(define g.current '())

(define sg.current (make-vector 100))

(define (g.current-extend! n)
  (let ((level (length g.current)))
    (set! g.current 
          (cons (cons n `(global . ,level)) g.current) )
    level ) )

(define (global-fetch i)
  (vector-ref sg.current i) )

(define (global-update! i v)
  (vector-set! sg.current i v) )

(define (g.current-initialize! name)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((global)
           (vector-set! sg.current (cdr kind) undefined-value) )
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (g.current-extend! name)))
          (vector-set! sg.current index undefined-value) ) ) )
  name )

;;; This tag is used in the value cell of uninitialized variables.

(define undefined-value (cons 'undefined 'value))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. G.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the SG.INIT vector.

(define g.init '())

(define sg.init (make-vector 100))

(define (predefined-fetch i)
  (vector-ref sg.init i) )

(define (g.init-extend! n)
  (let ((level (length g.init)))
    (set! g.init
          (cons (cons n `(predefined . ,level)) g.init) )
    level ) )

;;; Add that value is associated to name in the predefined global environment.

(define (g.init-initialize! name value)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((predefined)
           (vector-set! sg.init (cdr kind) value) )
          (else (static-wrong "Wrong redefinition" name)) )
        (let ((index (g.init-extend! name)))
          (vector-set! sg.init index value) ) ) )
  name )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Describe a predefined value.
;;; The description language only represents primitives with their arity:
;;;          (FUNCTION address . variables-list)
;;; with variables-list := () | (a) | (a b) | (a b c)
;;; Only the structure of the VARIABLES-LIST is interesting (not the
;;; names of the variables). ADDRESS is the address of the primitive
;;; to use when inlining an invokation to it. This address is
;;; represented by a Scheme procedure.

(define desc.init '())

(define (description-extend! name description)
  (set! desc.init 
        (cons (cons name description) desc.init) )
  name )

;;; Return the description or #f if absent.

(define (get-description name)
  (let ((p (assq name desc.init)))
    (and (pair? p) (cdr p)) ) )
        
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of functions. A redefinition with inlined vectors
;;; for more speed.
#|
(define-class closure Object
  ( code
    closed-environment
    ) )
|#

(define make-closure cons)

(define (invoke f v*)
  (if (closure? f)
      ((closure-code f) v* (closure-closed-environment f))
      (wrong "Not a function" f) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators
#|
(define (SHALLOW-ARGUMENT-REF j)
  (lambda () (activation-frame-argument *env* j)) )

(define (PREDEFINED i)
  (lambda () (predefined-fetch i)) )

(define (DEEP-ARGUMENT-REF i j)
  (lambda () (deep-fetch *env* i j)) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (lambda () (set-activation-frame-argument! *env* j (m))) )

(define (DEEP-ARGUMENT-SET! i j m)
  (lambda () (deep-update! *env* i j (m))) )

(define (GLOBAL-REF i)
  (lambda () (global-fetch i)) )

;;; Note that we lost the name of the variable, it must be retrieved
;;; from elsewhere.   TOBEDONE

(define (CHECKED-GLOBAL-REF i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable")
          v ) ) ) )

(define (GLOBAL-SET! i m)
  (lambda () (global-update! i (m))) )

(define (CONSTANT value)
  (lambda () value) )

(define (ALTERNATIVE m1 m2 m3)
  (lambda () (if (m1) (m2) (m3))) )

(define (SEQUENCE m m+)
  (lambda () (m) (m+)) )

(define (TR-FIX-LET m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (m+) ) )

(define (FIX-LET m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (let ((result (m+)))
      (set! *env* (environment-next *env*))
      result ) ) )

(define (CALL0 address)
  (lambda () (address)) )

(define (CALL1 address m1)
  (lambda () (address (m1))) )

(define (CALL2 address m1 m2)
  (lambda () (let ((v1 (m1))) 
               (address v1 (m2)) )) )

(define (CALL3 address m1 m2 m3)
  (lambda () (let* ((v1 (m1))
                    (v2 (m2)) )
               (address v1 v2 (m3)) )) )

(define (FIX-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (= (activation-frame-argument-length v*) arity+1)
            (begin (set! *env* (sr-extend* sr v*))
                   (m+) )
            (wrong "Incorrect arity") ) )
      (make-closure the-function *env*) ) ) )

(define (NARY-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (>= (activation-frame-argument-length v*) arity+1)
            (begin 
              (listify! v* arity)
              (set! *env* (sr-extend* sr v*))
              (m+) )
            (wrong "Incorrect arity") ) )
      (make-closure the-function *env*) ) ) )

(define (TR-REGULAR-CALL m m*) 
  (lambda ()
    (let ((f (m)))
      (invoke f (m*)) ) ) )

(define (REGULAR-CALL m m*)
  (lambda ()
    (let* ((f (m))
           (v* (m*))
           (sr *env*)
           (result (invoke f v*)) )
      (set! *env* sr)
      result ) ) )

(define (STORE-ARGUMENT m m* rank)
  (lambda ()
    (let* ((v (m))
           (v* (m*)) )
      (set-activation-frame-argument! v* rank v)
      v* ) ) )

(define (CONS-ARGUMENT m m* arity)
  (lambda ()
    (let* ((v (m))
           (v* (m*)) )
      (set-activation-frame-argument! 
       v* arity (cons v (activation-frame-argument v* arity)) )
      v* ) ) )

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (lambda ()
      (allocate-activation-frame size+1) ) ) )

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        v* ) ) ) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.
;;; E is the expression to evaluate
;;; SR is the representation of the local lexical environment
;;; TAIL? is a boolean that indicates if E is a terminal call (also
;;; means whether the *env* register should be restored or not).

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?) )
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        (else     (meaning-application (car e) (cdr e) r tail?)) ) ) )

(define (meaning-reference n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (SHALLOW-ARGUMENT-REF j)
                 (DEEP-ARGUMENT-REF i j) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (CHECKED-GLOBAL-REF i) ) )
          ((predefined)
           (let ((i (cdr kind)))
             (PREDEFINED i) ) ) )
        (static-wrong "No such variable" n) ) ) )

(define (meaning-quotation v r tail?)
  (CONSTANT v) )

(define (meaning-alternative e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r #f))
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)) )
    (ALTERNATIVE m1 m2 m3) ) )

(define (meaning-assignment n e r tail?) 
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)) )
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)) )
             (if (= i 0)
                 (SHALLOW-ARGUMENT-SET! j m)
                 (DEEP-ARGUMENT-SET! i j m) ) ) )
          ((global)
           (let ((i (cdr kind)))
             (GLOBAL-SET! i m) ) )
          ((predefined)
           (static-wrong "Immutable predefined variable" n) ) )
        (static-wrong "No such variable" n) ) ) )

(define (meaning-sequence e+ r tail?)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
          (meaning*-single-sequence (car e+) r tail?) )
      (static-wrong "Illegal syntax: (begin)") ) )

(define (meaning*-single-sequence e r tail?) 
  (meaning e r tail?) )

(define (meaning*-multiple-sequence e e+ r tail?)
  (let ((m1 (meaning e r #f))
        (m+ (meaning-sequence e+ r tail?)) )
    (SEQUENCE m1 m+) ) )

(define (meaning-abstraction nn* e+ r tail?)
  (let parse ((n* nn*)
              (regular '()) )
    (cond
     ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
     ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
     (else       (meaning-dotted-abstraction 
                  (reverse regular) n* e+ r tail? )) ) ) )

(define (meaning-fix-abstraction n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)) )
    (FIX-CLOSURE m+ arity) ) )

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)) )
    (NARY-CLOSURE m+ arity) ) )

;;; Application meaning.

(define (meaning-application e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     (let ((desc (get-description e)))
                       (and desc
                            (eq? 'function (car desc))
                            (or (= (length (cddr desc)) (length e*))
                                (static-wrong 
                                 "Incorrect arity for primitive" e )
                                ) ) ) ) ) )
         (meaning-primitive-application e e* r tail?) )
        ((and (pair? e)
              (eq? 'lambda (car e)) )
         (meaning-closed-application e e* r tail?) )
        (else (meaning-regular-application e e* r tail?)) ) )

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.

(define (meaning-closed-application e ee* r tail?)
  (let ((nn* (cadr e)))
    (let parse ((n* nn*)
                (e* ee*)
                (regular '()) )
      (cond
       ((pair? n*) 
        (if (pair? e*)
            (parse (cdr n*) (cdr e*) (cons (car n*) regular))
            (static-wrong "Too less arguments" e ee*) ) )
       ((null? n*)
        (if (null? e*)
            (meaning-fix-closed-application 
             nn* (cddr e) ee* r tail? )
            (static-wrong "Too much arguments" e ee*) ) )
       (else (meaning-dotted-closed-application 
              (reverse regular) n* (cddr e) ee* r tail? )) ) ) ) )

(define (meaning-fix-closed-application n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) #f))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail? (TR-FIX-LET m* m+) 
        (FIX-LET m* m+) ) ) )

(define (meaning-dotted-closed-application n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail? (TR-FIX-LET m* m+)
        (FIX-LET m* m+) ) ) )

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
;;; These primitives never change the *env* register nor have control effect.

(define (meaning-primitive-application e e* r tail?)
  (let* ((desc (get-description e))
         ;; desc = (function address . variables-list)
         (address (cadr desc))
         (size (length e*)) )
    (case size
      ((0) (CALL0 address))
      ((1) 
       (let ((m1 (meaning (car e*) r #f)))
         (CALL1 address m1) ) )
      ((2) 
       (let ((m1 (meaning (car e*) r #f))
             (m2 (meaning (cadr e*) r #f)) )
         (CALL2 address m1 m2) ) )
      ((3) 
       (let ((m1 (meaning (car e*) r #f))
             (m2 (meaning (cadr e*) r #f))
             (m3 (meaning (caddr e*) r #f)) )
         (CALL3 address m1 m2 m3) ) )
      (else (meaning-regular-application e e* r tail?)) ) ) )

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(define (meaning-regular-application e e* r tail?)
  (let* ((m (meaning e r #f))
         (m* (meaning* e* r (length e*) #f)) )
    (if tail? (TR-REGULAR-CALL m m*) (REGULAR-CALL m m*)) ) )

(define (meaning* e* r size tail?)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) r size tail?)
      (meaning-no-argument r size tail?) ) )

(define (meaning-dotted* e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-arguments (car e*) (cdr e*) 
                                     r size arity tail? )
      (meaning-no-dotted-argument r size arity tail?) ) )

(define (meaning-some-arguments e e* r size tail?)
  (let ((m (meaning e r #f))
        (m* (meaning* e* r size tail?))
        (rank (- size (+ (length e*) 1))) )
    (STORE-ARGUMENT m m* rank) ) )

(define (meaning-some-dotted-arguments e e* r size arity tail?)
  (let ((m (meaning e r #f))
        (m* (meaning-dotted* e* r size arity tail?))
        (rank (- size (+ (length e*) 1))) )
    (if (< rank arity) (STORE-ARGUMENT m m* rank)
        (CONS-ARGUMENT m m* arity) ) ) )

(define (meaning-no-argument r size tail?)
  (ALLOCATE-FRAME size) )

(define (meaning-no-dotted-argument r size arity tail?)
  (ALLOCATE-DOTTED-FRAME arity) )

;;; Gather into a list all arguments from arity+1 to the end of the
;;; activation frame and store this list into the arity+1th slot.

(define (listify! v* arity)
  (let loop ((index (- (activation-frame-argument-length v*) 1))
             (result '()) )
    (if (= arity index)
        (set-activation-frame-argument! v* arity result)
        (loop (- index 1)
              (cons (activation-frame-argument v* (- index 1))
                    result ) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Global environment initializers.

;;; Definitial allows to redefine immutable global variables. Useful
;;; when debugging interactively.

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (g.init-initialize! 'name value) ) ) )

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value 0)
     (defprimitive0 name value) )
    ((defprimitive name value 1)
     (defprimitive1 name value) )
    ((defprimitive name value 2)
     (defprimitive2 name value) )
    ((defprimitive name value 3)
     (defprimitive3 name value) ) ) )    
#|
(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda (v* sr)
                   (if (= (activation-frame-argument-length v*) 
                          arity+1 )
                       (value (activation-frame-argument v* 0))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         (make-closure behavior sr.init) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda (v* sr)
                   (if (= (activation-frame-argument-length v*)
                          arity+1 )
                       (value (activation-frame-argument v* 0) 
                              (activation-frame-argument v* 1) )
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a b))
         (make-closure behavior sr.init) ) ) ) ) )
|#
;;; Define a location in the user global environment.

#|
(define-syntax defvariable
  (syntax-rules ()
    ((defvariable name)
     (g.current-initialize! 'name) ) ) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.
#|
(definitial t #t)
(definitial f #f)
(definitial nil '())

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)
|#

;;; We do not need to save the register *env* since call/cc is not a
;;; primitive (it is defined by definitial and not by defprimitive)
;;; and non-primitive invokations are regularly handled.
#|
(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (v* sr)
       (if (= arity+1 (activation-frame-argument-length v*))
           (call/cc
            (lambda (k)
              (invoke 
               (activation-frame-argument v* 0)
               (let ((frame (allocate-activation-frame (+ 1 1))))
                 (set-activation-frame-argument! 
                  frame 0
                  (make-closure
                   (lambda (values r)
                     (if (= (activation-frame-argument-length values)
                            arity+1 )
                         (k (activation-frame-argument values 0))
                         (wrong "Incorrect arity" 'continuation) ) )
                   sr.init ) )
                 frame ) ) ) )
           (wrong "Incorrect arity" 'call/cc) ) )
     sr.init ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-closure
     (lambda (v* sr)
       (if (>= (activation-frame-argument-length v*) arity+1)
           (let* ((proc (activation-frame-argument v* 0))
                  (last-arg-index
                   (- (activation-frame-argument-length v*) 2) )
                  (last-arg 
                   (activation-frame-argument v* last-arg-index) )
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)) )
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument v* i) ) )
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)) )
                 ((null? last-arg))
               (set-activation-frame-argument! 
                frame i (car last-arg) ) )
             (invoke proc frame) )
           (wrong "Incorrect arity" 'apply) ) )
     sr.init ) ) )

(definitial list ((NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0)))
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Some free global locations:
#|
(defvariable x)
(defvariable y)
(defvariable z)
(defvariable a)
(defvariable b)
(defvariable c)
(defvariable foo)
(defvariable bar)
(defvariable hux)
(defvariable fib)
(defvariable fact)
(defvariable visit)
(defvariable length)
(defvariable primes)
|#

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Testing

(define (chapter63-interpreter)
  (define (toplevel)
    (set! *env* sr.init)
    (display ((meaning (read) r.init #t)))
    (toplevel) )
  (toplevel) )

;;; Preserve the current modifiable global environment (containing a,
;;; b, foo, fact, fib etc.) All tests will be compiled in that environment.

(define original.g.current
  (let ((g g.current))
    (lambda () g) ) )

;;; This variant produces a table of symbols.

(define sg.current.names (list 'foo))

(define (stand-alone-producer e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (global-names (map car (reverse g.current))) )
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (m) ) ) )


(define (CHECKED-GLOBAL-REF+ i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" 
                 (list-ref sg.current.names i) )
          v ) ) ) )  

;;; this one requires to close the name of the variables that must be
;;; checked. To use it you must also change meaning-reference that calls it.
#|
(define (CHECKED-GLOBAL-REF- i n)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" n)
          v ) ) ) ) 
|#
;;; retrofit for tests.
;; (set! CHECKED-GLOBAL-REF CHECKED-GLOBAL-REF+)
#|
(define (scheme6d)
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (set! *env* sr.init)
       (print ((stand-alone-producer (read)))) ) ) ) )

(define (test-scheme6d file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (check ((stand-alone-producer (read)))) ) )
   equal? ) )
|#
;;; Pay attention to tail-rec in Scheme->C.

(define (bench6d factor e)
  (let ((start (get-internal-run-time))
        (m (meaning e r.init #t)) )
    (let loop ((factor factor))
      (set! *env* sr.init)
      (let ((v (m)))
        (let ((duration (- (get-internal-run-time) start)))
          (when (<= factor 1)
            (display (list duration v))
            (newline) ) ) )
      (if (> factor 1)
          (loop (- factor 1)) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The following code use 
;;; pp to pretty-print expressions,
;;; and eval for a local hack (should be made of macros instead).

(define combinator-names
  '( SHALLOW-ARGUMENT-REF
     PREDEFINED
     DEEP-ARGUMENT-REF
     SHALLOW-ARGUMENT-SET!
     DEEP-ARGUMENT-SET!
     GLOBAL-REF
     CHECKED-GLOBAL-REF
     GLOBAL-SET!
     CONSTANT
     ALTERNATIVE
     SEQUENCE
     TR-FIX-LET
     FIX-LET
     CALL0
     CALL1
     CALL2
     CALL3
     FIX-CLOSURE
     NARY-CLOSURE
     TR-REGULAR-CALL
     REGULAR-CALL
     STORE-ARGUMENT
     CONS-ARGUMENT
     ALLOCATE-FRAME
     ALLOCATE-DOTTED-FRAME ) )

#|
(define install-regular-combinators
  (let ((originals (map eval combinator-names)))
    (lambda () 
      (for-each (lambda (old-value name)
                  (eval `(set! ,name ',old-value)) )
                originals
                combinator-names ) ) ) )
|#

(define (install-disassembling-combinators)
  (for-each (lambda (name)
              (eval `(set! ,name (lambda args (,name . ,args)))) )
            combinator-names ) )

(define (disassemble e)
  (install-disassembling-combinators)
  (pp (meaning e r.init #t))
  (install-regular-combinators)
  (newline) )

;;; (disassemble '(lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
;;; (disassemble '(lambda (n) (if (= n 0) 1 (* (fact (- n 1)) n))))


;;; Refinement of chap6d and chap7b. This interpreter introduces a
;;; *val* register and a *stack* to save/restore arguments that wait
;;; to be stored in an activation block. Functions now take their
;;; activation frame in the *val* register. Code is now a list of combinators. 

(define *val* #f)

(define *fun* #f)
(define *arg1* #f)
(define *arg2* #f)

(define *pc* '())

(define *stack* (make-vector 1000))
(define *stack-index* 0)

(define (stack-push v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)) )

(define (stack-pop)
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*) )

(define (save-stack)
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy ) )

(define (restore-stack copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#|
(define-class primitive Object
  ( address ) )

(define-class continuation Object
  ( stack 
    ) )
|#
(define make-primitive
  (lambda (v) (v)))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators
#|
(define (SHALLOW-ARGUMENT-REF j)
  (list (lambda () (set! *val* (activation-frame-argument *env* j)))) )

(define (PREDEFINED i)
  (list (lambda () (set! *val* (predefined-fetch i)))) )

(define (DEEP-ARGUMENT-REF i j)
  (list (lambda () (set! *val* (deep-fetch *env* i j)))) )

(define (SHALLOW-ARGUMENT-SET! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)) )

(define (SET-SHALLOW-ARGUMENT! j)
  (list (lambda () (set-activation-frame-argument! *env* j *val*))) )

(define (DEEP-ARGUMENT-SET! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)) )

(define (SET-DEEP-ARGUMENT! i j)
  (list (lambda () (deep-update! *env* i j *val*))) )

(define (GLOBAL-REF i)
  (list (lambda () (set! *val* (global-fetch i)))) )

(define (CHECKED-GLOBAL-REF i)
  (list (lambda () (set! *val* (global-fetch i))
                   (when (eq? *val* undefined-value)
                     (wrong "Uninitialized variable") ))) )

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)) )

(define (SET-GLOBAL! i)
  (list (lambda () (global-update! i *val*))) )

(define (CONSTANT value)
  (list (lambda () (set! *val* value))) )

(define (ALTERNATIVE m1 m2 m3)
  (append m1 (JUMP-FALSE (+ 1 (length m2)))
          m2 (GOTO (length m3)) 
          m3 ) )

(define (JUMP-FALSE i)
  (list (lambda () (if (not *val*) (set! *pc* (list-tail *pc* i))))) )

(define (GOTO i)
  (list (lambda () (set! *pc* (list-tail *pc* i)))) )

(define (SEQUENCE m m+)
  (append m m+) )

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+) )

(define (EXTEND-ENV)
  (list (lambda () (set! *env* (sr-extend* *env* *val*)))) )

(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)) )

(define (UNLINK-ENV)
  (list (lambda () (set! *env* (activation-frame-next *env*)))) )

(define (CALL0 address)
  (list (lambda () (set! *val* (address)))) )

(define (CALL1 address m1)
  (append m1 (INVOKE1 address) ) )

(define (INVOKE1 address)
  (list (lambda () (set! *val* (address *val*)))) )

(define (CALL2 address m1 m2)
  (append m1 (PUSH-VALUE) m2 (POP-ARG1) (INVOKE2 address)) )

(define (PUSH-VALUE)
  (list (lambda () (stack-push *val*))) )

(define (POP-ARG1)
  (list (lambda () (set! *arg1* (stack-pop)))) )

(define (INVOKE2 address)
  (list (lambda () (set! *val* (address *arg1* *val*)))) )

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE) 
          m2 (PUSH-VALUE) 
          m3 (POP-ARG2) (POP-ARG1) (INVOKE3 address) ) )

(define (POP-ARG2)
  (list (lambda () (set! *arg2* (stack-pop)))) )

(define (INVOKE3 address)
  (list (lambda () (set! *val* (address *arg1* *arg2* *val*)))) )

(define (FIX-CLOSURE m+ arity)
  (define the-function
    (append (ARITY=? (+ arity 1)) (EXTEND-ENV) m+ (RETURN)) )
  (append (CREATE-CLOSURE 1) (GOTO (length the-function)) 
          the-function ) )

(define (CREATE-CLOSURE offset)
  (list (lambda () (set! *val* (make-closure (list-tail *pc* offset) 
                                             *env* )))) )

(define (ARITY=? arity+1)
  (list (lambda () 
          (unless (= (activation-frame-argument-length *val*) arity+1)
            (wrong "Incorrect arity") ) )) )

(define (NARY-CLOSURE m+ arity)
  (define the-function
    (append (ARITY>=? (+ arity 1)) (PACK-FRAME! arity) (EXTEND-ENV)
            m+ (RETURN) ) )
  (append (CREATE-CLOSURE 1) (GOTO (length the-function)) 
          the-function ) )

(define (RETURN)
  (list (lambda () (set! *pc* (stack-pop)))) )

(define (PACK-FRAME! arity)
  (list (lambda () (listify! *val* arity))) )

(define (ARITY>=? arity+1)
  (list (lambda () 
          (unless (>= (activation-frame-argument-length *val*) arity+1)
            (wrong "Incorrect arity") ) )) )

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE) m* (POP-FUNCTION) (FUNCTION-INVOKE)) )

(define (POP-FUNCTION)
  (list (lambda () (set! *fun* (stack-pop)))) )

(define (FUNCTION-INVOKE)
  (list (lambda () (invoke *fun*))) )

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE) 
          m* (POP-FUNCTION) (PRESERVE-ENV) 
             (FUNCTION-INVOKE) (RESTORE-ENV)
          ) )

(define (PRESERVE-ENV)
  (list (lambda () (stack-push *env*))) )

(define (RESTORE-ENV)
  (list (lambda () (set! *env* (stack-pop)))) )

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)) )

(define (POP-FRAME! rank)
  (list (lambda () (set-activation-frame-argument! *val* rank (stack-pop)))) )

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)) )

(define (POP-CONS-FRAME! arity)
  (list (lambda () 
          (set-activation-frame-argument! 
           *val* arity (cons (stack-pop)
                             (activation-frame-argument *val* arity) ) ) )) )

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (list (lambda () (set! *val* (allocate-activation-frame size+1)))) ) )

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (list (lambda ()
            (let ((v* (allocate-activation-frame arity+1)))
              (set-activation-frame-argument! v* arity '())
              (set! *val* v*) ) )) ) )

(define (FINISH)
  (list (lambda () (*exit* *val*))) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (invoke f)
  (cond ((closure? f)
         (stack-push *pc*)
         (set! *env* (closure-closed-environment f))
         (set! *pc* (closure-code f)) )
        ((primitive? f)
         ((primitive-address f)) )
        ((continuation? f)
         (if (= (+ 1 1) (activation-frame-argument-length *val*))
             (begin
               (restore-stack (continuation-stack f))
               (set! *val* (activation-frame-argument *val* 0))
               (set! *pc* (stack-pop)) )
             (wrong "Incorrect arity" 'continuation) ) )
        (else (wrong "Not a function" f)) ) )
      
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#|
(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (set! *val* (value (activation-frame-argument *val* 0)))
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a))
         (make-primitive behavior) ) ) ) ) )
  
(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda ()
                   (show-registers 'name) ;; debug
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (set! *val* 
                             (value (activation-frame-argument *val* 0) 
                                    (activation-frame-argument *val* 1) ) )
                       (wrong "Incorrect arity" 'name) ) ) ) )
         (description-extend! 'name `(function ,value a b))
         (make-primitive behavior) ) ) ) ) )

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)
|#

(define wrong
  (lambda (msg proc)
    (display msg)
    (display proc)
    (newline)))

#|
(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((f (activation-frame-argument *val* 0))
                 (frame (allocate-activation-frame (+ 1 1))))
             (stack-push *pc*)
             (set-activation-frame-argument! 
              frame 0 (make-continuation (save-stack)) )
             (stack-pop)
             (set! *val* frame)
             (invoke f) )
           (wrong "Incorrect arity" 'call/cc) ) ) ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let* ((proc (activation-frame-argument *val* 0))
                  (last-arg-index (- (activation-frame-argument-length *val*) 2))
                  (last-arg (activation-frame-argument *val* last-arg-index))
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)) )
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument *val* i) ) )
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)) )
                 ((null? last-arg))
               (set-activation-frame-argument! frame i (car last-arg)) )
             (set! *val* frame)
             (invoke proc) )
           (wrong "Incorrect arity" 'apply) ) ) ) ) )

(definitial list
  (make-primitive
   (lambda ()
     (let ((args-number (- (activation-frame-argument-length *val*) 1))
           (result '()) )
       (do ((i args-number (- i 1)))
           ((= i 0))
         (set! result (cons (activation-frame-argument *val* (- i 1)) 
                            result )) ) 
       (set! *val* result) ) ) ) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define *debug* #f)

(define (show-registers message)
  (and *debug* (format #t "
----------------~A
PC    = -~A
ENV   = ~A
VAL   = ~A
FUN   = ~A
STACK = ~A~%" message (length *pc*)
              *env* *val* *fun* (save-stack) ) )
  )

(define (run)
  (let ((instruction (car *pc*)))
    (set! *pc* (cdr *pc*))
    (instruction)
    (run) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

#|
(define install-regular-combinators
  (let ((originals (map eval combinator-names)))
    (lambda () 
      (for-each (lambda (old-value name)
                  (eval `(set! ,name ',old-value)) )
                originals
                combinator-names ) ) ) )
|#

(define (install-disassembling-combinators)
  (for-each (lambda (name)
              (eval `(set! ,name (lambda args (,name . ,args)))) )
            combinator-names ) )

(define combinator-names
  '( SHALLOW-ARGUMENT-REF
     PREDEFINED
     DEEP-ARGUMENT-REF
     SET-SHALLOW-ARGUMENT
     SET-DEEP-ARGUMENT!
     GLOBAL-REF
     CHECKED-GLOBAL-REF
     SET-GLOBAL!
     CONSTANT
     JUMP-FALSE
     GOTO
     EXTEND-ENV
     UNLINK-ENV
     CALL0
     INVOKE1
     PUSH-VALUE
     POP-ARG1
     INVOKE2
     POP-ARG2
     INVOKE3
     CREATE-CLOSURE
     ARITY=?
     RETURN
     PACK-FRAME!
     ARITY>=?
     POP-FUNCTION
     FUNCTION-INVOKE
     PRESERVE-ENV
     RESTORE-ENV
     POP-FRAME!
     POP-CONS-FRAME!
     ALLOCATE-FRAME
     ALLOCATE-DOTTED-FRAME
     FINISH
    ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (chapter7c-interpreter)
  (define (toplevel)
    (define e (read))
    (set! *env* sr.init)
    (set! *val* #f)
    (set! *fun* #f)
    (set! *arg1* #f)
    (set! *arg2* #f)
    (set! *stack-index* 0)
    (set! *pc* (append (meaning e r.init #t)
                       (FINISH) ))
    (when *debug* (disassemble e) (display *pc*) (newline)) ;; DEBUG
    (call/cc (lambda (exit)
               (set! *exit* exit)
               (run) ))
    (display *val*)
    (toplevel) )
  (toplevel) )

(define (stand-alone-producer7c e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init #t))
         (size (length g.current))
         (global-names (map car (reverse g.current))) )
    (when *debug* (disassemble e)) ;; DEBUG
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (set! *val* #f)
      (set! *fun* #f)
      (set! *arg1* #f)
      (set! *arg2* #f)
      (set! *stack-index* 0)
      (set! *pc* (append m (FINISH)))
      ;;(display m)(newline) ;; debug
      (call/cc (lambda (exit)
                 (set! *exit* exit)
                 (run) )) ) ) )    

(define (test-scheme7c file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       ((stand-alone-producer7c (read)))
       (check *val*) ) )
   equal? ) )

;;; Missing definitions
(define *exit* #f)

;;; end of chap7c.scm

<;;; $Id: chap7d.scm,v 4.2 2006/11/24 18:16:32 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;; Refinement of chap6d and chap7c. This interpreter introduces a
;;; *val* register and a *stack* to save/restore arguments that wait
;;; to be stored in an activation block. Functions now take their
;;; activation frame in the *val* register. Code is now a list of
;;; bytes.

;;; Load chap6d before.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The runtime machine

;(define *env* #f) ; already appears in chap6d
(define *val* #f)
(define *fun* #f)
(define *arg1* #f)
(define *arg2* #f)

(define *pc* 0)
(define *code* (vector 20))

(define *constants* (vector))

;;; Some tests depend on 100 being the depth of the stack.
(define *stack* (make-vector 100))
(define *stack-index* 0)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (stack-push v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)) )

(define (stack-pop)
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*) )

(define (save-stack)
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy ) )

(define (restore-stack copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

(define (quotation-fetch i)
  (vector-ref *constants* i) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; make them inherit from invokable.
#|
(define-class primitive Object
  ( address ) )

(define-class continuation Object
  ( stack ) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; This global variable holds at preparation time all the interesting
;;; quotations. It will be converted into *constants* for run-time.
;;; Quotations are not compressed and can appear multiply.

(define *quotations* (list))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators that just expand into instructions.

(define (SHALLOW-ARGUMENT-SET! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)) )

(define (DEEP-ARGUMENT-SET! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)) )

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)) )

;;; GOTO is not necessary if m2 is a tail-call but don't care.
;;; This one changed since chap7c.scm

(define (ALTERNATIVE m1 m2 m3)
  (let ((mm2 (append m2 (GOTO (length m3)))))
    (append m1 (JUMP-FALSE (length mm2)) mm2 m3) ) )

(define (SEQUENCE m m+)
  (append m m+) )

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+) )

(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)) )

(define (CALL0 address)
  (INVOKE0 address) )

(define (CALL1 address m1)
  (append m1 (INVOKE1 address) ) )

(define (CALL2 address m1 m2)
  (append m1 (PUSH-VALUE) m2 (POP-ARG1) (INVOKE2 address)) )

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE) 
          m2 (PUSH-VALUE) 
          m3 (POP-ARG2) (POP-ARG1) (INVOKE3 address) ) )

(define (FIX-CLOSURE m+ arity)
  (let* ((the-function (append (ARITY=? (+ arity 1)) (EXTEND-ENV)
                               m+  (RETURN) ))
         (the-goto (GOTO (length the-function))) )
    (append (CREATE-CLOSURE (length the-goto)) the-goto the-function) ) )

(define (NARY-CLOSURE m+ arity)
  (let* ((the-function (append (ARITY>=? (+ arity 1)) (PACK-FRAME! arity)
                               (EXTEND-ENV) m+ (RETURN) ))
         (the-goto (GOTO (length the-function))) )
    (append (CREATE-CLOSURE (length the-goto)) the-goto the-function) ) )

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE) m* (POP-FUNCTION) (FUNCTION-GOTO)) )

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE) m* (POP-FUNCTION) 
          (PRESERVE-ENV) (FUNCTION-INVOKE) (RESTORE-ENV) ) )

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)) )

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Instructions definers

(define-syntax define-instruction-set
  (syntax-rules (define-instruction)
    ((define-instruction-set
       (define-instruction (name . args) n . body) ... )
     (begin 
       (define (run)
         (let ((instruction (fetch-byte)))
           (case instruction
             ((n) (run-clause args body)) ... ) )
         (run) )
       (define (instruction-size code pc)
         (let ((instruction (vector-ref code pc)))
           (case instruction
             ((n) (size-clause args)) ... ) ) )
       (define (instruction-decode code pc)
         (define (fetch-byte)
           (let ((byte (vector-ref code pc)))
             (set! pc (+ pc 1))
             byte ) )
         (let-syntax
             ((decode-clause
               (syntax-rules ()
                 ((decode-clause iname ()) '(iname))
                 ((decode-clause iname (a)) 
                  (let ((a (fetch-byte))) (list 'iname a)) )
                 ((decode-clause iname (a b))
                  (let* ((a (fetch-byte))(b (fetch-byte)))
                    (list 'iname a b) ) ) )))
           (let ((instruction (fetch-byte)))
             (case instruction
               ((n) (decode-clause name args)) ... ) ) ) ) ) ) ) )

;;; This uses the global fetch-byte function that increments *pc*.

(define-syntax run-clause
  (syntax-rules ()
    ((run-clause () body) (begin . body))
    ((run-clause (a) body)
     (let ((a (fetch-byte))) . body) )
    ((run-clause (a b) body)
     (let* ((a (fetch-byte))(b (fetch-byte))) . body) ) ) )           

(define-syntax size-clause
  (syntax-rules ()
    ((size-clause ())    1)
    ((size-clause (a))   2)
    ((size-clause (a b)) 3) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Instruction-set. 
;;; The instructions are kept in a separate file (to be read by
;;; LiSP2TeX), the following macro reads them and generates a
;;; (define-instruction-set...) call.                       HACK!
;;; You can replace this paragraphe by:
;;;  (define-instruction-set <content of "src/chap7f.scm" file>...)
#|
(define-abbreviation (definstructions filename)
  (define (read-file filename)
    (call-with-input-file filename
      (lambda (in)
        (let gather ((e (read in))
                     (content '()) )
          (if (eof-object? e)
              (reverse content)
              (gather (read in) (cons e content)) ) ) ) ) )
  (let ((content (read-file filename)) )
    (display '(reading instruction set ...))(newline)
    `(define-instruction-set . ,content) ) )

(definstructions "src/chap7f.scm")
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(define (check-byte j)
  (or (and (<= 0 j) (<= j 255))
    (static-wrong "Cannot pack this number within a byte" j) ) )
  
(define (SHALLOW-ARGUMENT-REF j)
  (check-byte j)
  (case j
    ((0 1 2 3) (list (+ 1 j)))
    (else      (list 5 j)) ) )

(define (PREDEFINED i)
  (check-byte i)
  (case i
    ;; 0=\#t, 1=\#f, 2=(), 3=cons, 4=car, 5=cdr, 6=pair?, 7=symbol?, 8=eq?
    ((0 1 2 3 4 5 6 7 8) (list (+ 10 i)))
    (else                (list 19 i)) ) )

(define (DEEP-ARGUMENT-REF i j) (list 6 i j))

(define (SET-SHALLOW-ARGUMENT! j)
  (case j
    ((0 1 2 3) (list (+ 21 j)))
    (else      (list 25 j)) ) )

(define (SET-DEEP-ARGUMENT! i j) (list 26 i j))

(define (GLOBAL-REF i) (list 7 i))

(define (CHECKED-GLOBAL-REF i) (list 8 i))

(define (SET-GLOBAL! i) (list 27 i))

(define (CONSTANT value)
  (cond ((eq? value #t)    (list 10))
        ((eq? value #f)    (list 11))
        ((eq? value '())   (list 12))
        ((equal? value -1) (list 80))
        ((equal? value 0)  (list 81))
        ((equal? value 1)  (list 82))
        ((equal? value 2)  (list 83))
        ((equal? value 4)  (list 84))
        ((and (integer? value)  ; immediate value
              (<= 0 value)
              (< value 255) )
         (list 79 value) )
        (else (EXPLICIT-CONSTANT value)) ) )

(define (EXPLICIT-CONSTANT value)
  (set! *quotations* (append *quotations* (list value)))
  (list 9 (- (length *quotations*) 1)) )

;;; All gotos have positive offsets (due to the generation)

(define (GOTO offset)
  (cond ((< offset 255) (list 30 offset))
        ((< offset (+ 255 (* 255 256))) 
         (let ((offset1 (modulo offset 256))
               (offset2 (quotient offset 256)) )
           (list 28 offset1 offset2) ) )
        (else (static-wrong "too long jump" offset)) ) )

(define (JUMP-FALSE offset)
  (cond ((< offset 255) (list 31 offset))
        ((< offset (+ 255 (* 255 256))) 
         (let ((offset1 (modulo offset 256))
               (offset2 (quotient offset 256)) )
           (list 29 offset1 offset2) ) )
        (else (static-wrong "too long jump" offset)) ) )

(define (EXTEND-ENV) (list 32))

(define (UNLINK-ENV) (list 33))

(define (INVOKE0 address)
  (case address
    ((read)    (list 89))
    ((newline) (list 88))
    (else (static-wrong "Cannot integrate" address)) ) )

(define (INVOKE1 address)
  (case address
    ((car)     (list 90))
    ((cdr)     (list 91))
    ((pair?)   (list 92))
    ((symbol?) (list 93))
    ((display) (list 94))
    (else (static-wrong "Cannot integrate" address)) ) )

;;; The same one with other unary primitives.
(define (INVOKE1 address)
  (case address
    ((car)     (list 90))
    ((cdr)     (list 91))
    ((pair?)   (list 92))
    ((symbol?) (list 93))
    ((display) (list 94))
    ((primitive?) (list 95))
    ((null?)   (list 96))
    ((continuation?) (list 97))
    ((eof-object?)   (list 98))
    (else (static-wrong "Cannot integrate" address)) ) )

(define (PUSH-VALUE) (list 34)) 

(define (POP-ARG1) (list 35))

(define (INVOKE2 address)
  (case address
    ((cons)     (list 100))
    ((eq?)      (list 101))
    ((set-car!) (list 102))
    ((set-cdr!) (list 103))
    ((+)        (list 104))
    ((-)        (list 105))
    ((=)        (list 106))
    ((<)        (list 107))
    ((>)        (list 108))
    ((*)        (list 109))
    ((<=)       (list 110))
    ((>=)       (list 111))
    ((remainder)(list 112))
    (else (static-wrong "Cannot integrate" address)) ) )

(define (POP-ARG2) (list 36))

(define (INVOKE3 address)
  (static-wrong "No ternary integrated procedure" address) )

(define (CREATE-CLOSURE offset) (list 40 offset))

(define (ARITY=? arity+1)
  (case arity+1
    ((1 2 3 4) (list (+ 70 arity+1)))
    (else        (list 75 arity+1)) ) )

(define (RETURN) (list 43))

(define (PACK-FRAME! arity) (list 44 arity))

(define (ARITY>=? arity+1) (list 78 arity+1))

(define (FUNCTION-GOTO) (list 46))

(define (POP-FUNCTION) (list 39))

(define (FUNCTION-INVOKE) (list 45))

(define (PRESERVE-ENV) (list 37))

(define (RESTORE-ENV) (list 38))

(define (POP-FRAME! rank)
  (case rank
    ((0 1 2 3) (list (+ 60 rank)))
    (else      (list 64 rank)) ) )

(define (POP-CONS-FRAME! arity) (list 47 arity))

(define (ALLOCATE-FRAME size)
  (case size
    ((0 1 2 3 4) (list (+ 50 size)))
    (else        (list 55 (+ size 1))) ) )

(define (ALLOCATE-DOTTED-FRAME arity) (list 56 (+ arity 1)))

(define (FINISH) (list 20))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Preserve the state of the machine ie the three environments.

(define (preserve-environment)
  (stack-push *env*) )

(define (restore-environment)
  (set! *env* (stack-pop)) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (fetch-byte)
  (let ((byte (vector-ref *code* *pc*)))
    (set! *pc* (+ *pc* 1))
    byte ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Disassemble code

(define (disassemble code)
  (let loop ((result '())
             (pc 0) )
    (if (>= pc (vector-length code))
        (reverse! result)
        (loop (cons (instruction-decode code pc) result)
              (+ pc (instruction-size code pc)) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; If tail? is #t then the return address is on top of stack so no
;;; need to push another one.
#|
(define-generic (invoke (f) tail?)
  (signal-exception #f (list "Not a function" f)) )

(define-method (invoke (f closure) tail?)
  (unless tail? (stack-push *pc*))
  (set! *env* (closure-closed-environment f))
  (set! *pc* (closure-code f)) )

(define-method (invoke (f primitive) tail?)
  (unless tail? (stack-push *pc*))
  ((primitive-address f)) )

(define-method (invoke (f continuation) tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (begin
        (restore-stack (continuation-stack f))
        (set! *val* (activation-frame-argument *val* 0))
        (set! *pc* (stack-pop)) )
      (signal-exception #f (list "Incorrect arity" 'continuation)) ) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
  
(define-syntax defprimitive0
  (syntax-rules ()
    ((defprimitive0 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 0))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (begin
                         (set! *val* (value))
                         (set! *pc* (stack-pop)) )
                       (signal-exception #t (list "Incorrect arity" 'name)) ) ) ) )
         (description-extend! 'name `(function value))
         (make-primitive behavior) ) ) ) ) )
  
(define-syntax defprimitive1
  (syntax-rules ()
    ((defprimitive1 name value)
     (definitial name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (let ((arg1 (activation-frame-argument *val* 0)))
                         (set! *val* (value arg1))
                         (set! *pc* (stack-pop)) )
                       (signal-exception #t (list "Incorrect arity" 'name)) ) ) ) )
         (description-extend! 'name `(function value a))
         (make-primitive behavior) ) ) ) ) )

(define-syntax defprimitive2
  (syntax-rules ()
    ((defprimitive2 name value)
     (definitial name
       (letrec ((arity+1 (+ 2 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (let ((arg1 (activation-frame-argument *val* 0))
                             (arg2 (activation-frame-argument *val* 1)) )
                         (set! *val* (value arg1 arg2))
                         (set! *pc* (stack-pop)) )
                       (signal-exception #t (list "Incorrect arity" 'name)) ) ) ) )
         (description-extend! 'name `(function value a b))
         (make-primitive behavior) ) ) ) ) )

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)
(defprimitive read read 0)
(defprimitive primitive? primitive? 1)
(defprimitive continuation? continuation? 1)
(defprimitive null? null? 1)
(defprimitive newline newline 0)
(defprimitive eof-object? eof-object? 1)

;;; The function which is invoked by call/cc always waits for an
;;; activation frame. 

;; TODO
(define signal-exception
  (lambda (b ls)
    1))

#|
(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((f (activation-frame-argument *val* 0))
                 (frame (allocate-activation-frame (+ 1 1))) )
             (set-activation-frame-argument! 
              frame 0 (make-continuation (save-stack)) )
             (set! *val* frame)
             (set! *fun* f)             ; useful for debug
             (invoke f #t) )
           (signal-exception #t (list "Incorrect arity" 
                                      'call/cc )) ) ) ) ) )

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)) )
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let* ((proc (activation-frame-argument *val* 0))
                  (args-number (activation-frame-argument-length *val*))
                  (last-arg-index (- args-number 2))
                  (last-arg (activation-frame-argument *val* last-arg-index))
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)) )
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument *val* i) ) )
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)) )
                 ((null? last-arg))
               (set-activation-frame-argument! frame i (car last-arg)) )
             (set! *val* frame)
             (set! *fun* proc)  ; useful for debug
             (invoke proc #t) )
           (signal-exception #f (list "Incorrect arity" 'apply)) ) ) ) ) )

(definitial list
  (make-primitive
   (lambda ()
     (let ((args-number (- (activation-frame-argument-length *val*) 1))
           (result '()) )
       (do ((i args-number (- i 1)))
           ((= i 0))
         (set! result (cons (activation-frame-argument *val* (- i 1)) 
                            result )) ) 
       (set! *val* result)
       (set! *pc* (stack-pop)) ) ) ) )
|#
;;; Reserve some variables for future use in future chapters.

#|
(define-syntax defreserve
  (syntax-rules ()
    ((defreserve name)
     (definitial name
       (make-primitive
        (lambda ()
          (signal-exception #f (list "Not yet implemented" 'name)) ) ) ) ) ) )

(defreserve global-value)
(defreserve load)
(defreserve eval)
(defreserve eval/at)
(defreserve eval/b)
(defreserve enrich)
(defreserve procedure->environment)
(defreserve procedure->definition)
(defreserve variable-value)
(defreserve set-variable-value!)
(defreserve variable-defined?)
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Use Meroon show functions to describe the inner working.

(define *debug* #f)

(define (show-registers message)
  (when *debug* 
    (format #t "~%----------------~A" message)
    (format #t "~%ENV  = ") (show *env*)
    (format #t "~%VAL  = ") (show *val*)
    (format #t "~%FUN  = ") (show *fun*)
    (show-stack (save-stack))
    (format #t "~%(PC  = ~A), next INSTR to be executed = ~A~%" 
            *pc* (instruction-decode *code* *pc*) ) ) )

(define (show-stack stack)
  (let ((n (vector-length stack)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (format #t "~%STK[~A]= " i)(show (vector-ref *stack* i)) ) ) )

#|
(define-method (show (f closure) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (format stream "#<Closure(pc=~A)>" (closure-code f)) ) )

(define-method (show (a activation-frame) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "[Frame next=" stream)
    (show (activation-frame-next a) stream)
    (display ", content=" stream)
    (do ((i 0 (+ 1 i)))
        ((= i (activation-frame-argument-length a)))
      (show (activation-frame-argument a i) stream)
      (display " & " stream) )
    (display "]" stream) ) )
|#
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(define (code-prologue)
  (set! finish-pc 0)
  (FINISH) )

(define (make-code-segment m)
  (apply vector (append (code-prologue) m (RETURN))) )

(define (chapter7d-interpreter)
  (define (toplevel)
    (display ((stand-alone-producer7d (read)) 100))
    (toplevel) )
  (toplevel) ) 

(define (stand-alone-producer7d e)
  (set! g.current (original.g.current))
  (set! *quotations* '())
  (let* ((code (make-code-segment (meaning e r.init #t)))
         (start-pc (length (code-prologue)))
         (global-names (map car (reverse g.current)))
         (constants (apply vector *quotations*)) )
    (lambda (stack-size)
      (run-machine stack-size start-pc code 
                   constants global-names ) ) ) )

(define (run-machine stack-size pc code constants global-names)
  (set! sg.current (make-vector (length global-names) 
                                undefined-value ))
  (set! sg.current.names global-names)
  (set! *constants*   constants)
  (set! *code*        code)
  (set! *env*         sr.init)
  (set! *stack*       (make-vector stack-size))
  (set! *stack-index* 0)
  (set! *val*         'anything)
  (set! *fun*         'anything)
  (set! *arg1*        'anything)
  (set! *arg2*        'anything)
  (stack-push finish-pc)                ;  pc for FINISH
  (set! *pc*          pc)
  (call/cc (lambda (exit)
             (set! *exit* exit)
             (run) )) )

;;; Patch run to show registers in debug mode.

(let ((native-run run))
  (set! run (lambda ()
              (when *debug* (show-registers ""))
              (native-run) )) )
(let ((native-run-machine run-machine))
  (set! run-machine
        (lambda (stack-size pc code constants global-names)
          (when *debug*                     ; DEBUG
            (format #t "Code= ~A~%" (disassemble code)) )         
          (native-run-machine stack-size pc code constants global-names) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests

(define (scheme7d)
  (interpreter
   "Scheme? "  
   "Scheme= " 
   #t
   (lambda (read print error)
     (setup-wrong-functions error)
     (lambda ()
       ((stand-alone-producer7d (read)) 100)
       (print *val*) ) ) ) )

(define (test-scheme7d file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (setup-wrong-functions error)
     (lambda ()
       ((stand-alone-producer7d (read)) 100)
       (check *val*) ) )
   equal? ) )

(define (setup-wrong-functions error)
  (set! signal-exception (lambda (c . args) (apply error args)))
  (set! wrong (lambda args
                (format #t "
		>>>>>>>>>>>>>>>>>>RunTime PANIC<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" (activation-frame-argument *val* 1) )
                (apply error args) ))
  (set! static-wrong (lambda args
                       (format #t "
		>>>>>>>>>>>>>>>>>>Static WARNING<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" args )
                       (apply error args) )) )

;;; Missing global variables

(define signal-exception 'wait)
(define finish-pc 'wait)
(define *exit* 'wait)