(func cps
      ast cc => (list cc ast) where (or (symbol? ast) (number? ast) (string? ast) (boolean? ast))
      (list 'if a b c) cc => (cps a (let ((r1 (gensym)))
                                      (list 'lambda (list r1)
                                            (list 'if r1
                                                  (cps b cc)
                                                  (cps c cc)))))
      (list 'begin e1 e2) cc => (cps e1 (list 'lambda (list (gensym))
                                              (cps e2 cc)))
      (list 'set! v e) cc => (cps e (let ((r1 (gensym)))
                                      (list 'lambda (list r1)
                                            (list cc (list 'set! v r1)))))
      (list 'lambda args e) cc => (let ((k (gensym)))
                                    (list cc (list 'lambda (cons k args)
                                                   (cps e k))))
      (list e) cc => (cps e (let ((r0 (gensym)))
                              (list 'lambda (list r0)
                                    (list r0 cc))))
      (list op x y) cc => (let ((r1 (gensym))
                                (r2 (gensym)))
                            (cps x
                                 (list 'lambda (list r1)
                                       (cps y
                                            (list 'lambda (list r2)
                                                  (list cc (list op r1 r2))))))) where
                                                  (memq op '(= * + - / >))
      (list e0 e1) cc => (cps e0 (let ((r0 (gensym))
                                       (r1 (gensym)))
                                   (list 'lambda (list r0)
                                         (cps e1
                                              (list 'lambda (list r1)
                                                    (list r0 cc r1)))))))

(defun cps-convert (ast)
  (cps ast '(lambda (x) x)))

;; (define id (lambda (x) x))

;; (cps '(set! fact
;;   (lambda (n)
;;     (if (= n 0)
;;         1
;;         (* n (fact (- n 1)))
;;        )))
;;      'id)

;; (fact id 10)

(func diff
      '() s2 => '()
      (cons x y) s2 => (if (memq x s2)
                           (diff y s2)
                           (cons x (diff y s2))))

(func union
      '() s2 => s2
      (cons x y) s2 => (if (memq x s2)
                           (union y s2)
                           (cons x (union y s2))))

(func free-vars
      ast => (list ast) where (symbol? ast)
      (list 'set! x v) => (union (free-vars v) (list x))
      (list-rest 'if more) => (foldl union '() (map free-vars more))
      (list 'lambda args e) => (diff (free-vars e) args)
      ast => (foldl union '() (map free-vars ast)))

(func pos-in-list0
      x '() _ => -1
      x (cons y l) i => (if (eq? x y)
                            i
                            (pos-in-list0 x l (+ i 1))))

(defun pos-in-list (x l) (pos-in-list0 x l 0))

(defun not-global (x) (not (memq x '(+ - * / > < = eq?))))

(func convert
      ast self free => ast where (or (number? ast) (string? ast) (boolean? ast))
      ast self free =>
      (let ((pos (pos-in-list ast free)))
        (if (= pos -1)
            ast
            (list '%closure-ref self (+ pos 1)))) where (symbol? ast)
      (list 'set! x v) self free => (list 'set! x (convert v self free))
      (list 'if a b c) self free => (list 'if (convert a self free)
                                          (convert b self free)
                                          (convert c self free))
      (list 'lambda args e) self free =>
      (let ((fv (filter not-global (free-vars (list 'lambda args e))))
            (self-var (gensym))
            (fn (lambda (x) (convert x self free))))
        (let ((lam (list 'lambda (cons self-var args)
                         (convert e self-var fv)))
              (new-fv (map fn fv)))
          (cons '%closure (cons lam new-fv))))
      ;; an optimize, no need to closure-convert
      (list-rest (list 'lambda params body) args) self free =>
      (cons (list 'lambda params (convert body self free))
              (map (lambda (x) (convert x self free)) args))
      (list-rest op args) self free =>
      (cons op (map (lambda (x) (convert x self free)) args)) where (memq op '(+ - * / > < = eq?))
      funcall self free => (cons '%apply (map (lambda (x) (convert x self free)) funcall)))

(defun closure-convert (ast)
  (convert ast #f '()))
