(define cps
  AST CC -> [%apply CC [%lit AST]] where (or (number? AST) (string? AST) (boolean? AST))
  AST CC -> [%apply CC AST] where (symbol? AST)
  [if A B C] CC -> (cps a (let R1 (gensym r)
                                [%lambda [R1]
                                  [%if R1 (cps B CC)
                                          (cps C CC)]]))
  [begin E1 E2] CC -> (cps E1 [%lambda [(gensym _)]
                                        (cps E2 CC)])
  [set! V E] CC -> (cps E (let R1 (gensym r)
                                [%lambda [R1]
                                        [%apply CC [%apply [%prim set!] V R1]]]))
  [lambda ARGS E] CC -> (let K (gensym k)
                              [%apply CC [%lambda (cons K ARGS)
                                          (cps E K)]])
  [E] CC -> (cps E (let R0 (gensym r)
                        [%lambda [R0] [%apply R0 CC]]))
  [OP X Y] CC -> (let R1 (gensym r)
                      R2 (gensym r)
                      (cps X
                            [%lambda [R1]
                                    (cps Y
                                        [%lambda [R2]
                                              [%apply CC [%apply [%prim OP] R1 R2]]])]))
                  where (element? OP [= * + - / >])
  [E0 E1] CC -> (cps E0 (let R0 (gensym r)
                              R1 (gensym r)
                                [%lambda [R0]
                                      (cps E1
                                          [%lambda [R1]
                                                    [%apply R0 CC R1]])])))

(defun cps-convert (AST)
  (cps AST [%lambda [X] X]))

(define gen-let
  Res [] [] -> (reverse Res)
  Res [X | Y] [A | B] -> (gen-let [[X A] | Res] Y B))

(define pp-ast
  [%apply Var | L] -> [[%closure-ref Var 0] Var | (map (pp-ast) L)] where (symbol? Var)
  [%apply [%lambda Params Body] | Args] -> (let NArgs (map (pp-ast) Args)
                                                Bind (gen-let [] Params NArgs)
                                                [let Bind (pp-ast Body)])
  [%apply | L] -> (map (pp-ast) L)
  [%lambda ARGS BODY] -> [lambda ARGS (pp-ast BODY)]
  [%closure | L] -> [%closure | (map (pp-ast) L)]
  [%prim X] -> X
  [%lit X] -> X
  [%if | L] -> [if | (map (pp-ast) L)]
  X -> X)


(defun source (X)
  (make-string "~R" (pp-ast X)))

(define diff
  S1 [] -> S1
  [] S2 -> []
  [X | Y] S2 -> (diff Y S2) where (element? X S2)
  [X | Y] S2 -> [X | (diff Y S2)])

(define foldl
  F Init [X | Y] -> (foldl F (F X Init) Y)
  F Init [] -> Init)

(define free-vars
  AST -> [AST] where (symbol? AST)
  [%lit X] -> []
  [%prim OP] -> []
  [%if | More] -> (foldl (union) [] (map (free-vars) More))
  [%lambda ARGS BODY] -> (diff (free-vars BODY) ARGS)
  [%apply | More] -> (foldl (union) [] (map (free-vars) More)))

(define pos-in-list0
  X [] _ -> -1
  X [X | L] I -> I
  X [_ | L] I -> (post-in-list0 X L (+ I 1)))

(defun pos-in-list (x l)
  (pos-in-list0 x l 0))

(define convert
  SELF FREE [%prim OP]  -> [%prim OP]
  SELF FREE [%lit AST]  -> [%lit AST]

  SELF FREE AST  -> (let POS (pos-in-list AST FREE)
                           (if (= POS -1)
                               AST
                             [%closure-ref SELF (+ pos 1)])) where (symbol? AST)

  SELF FREE [%if | More] -> [%if | (map (convert SELF FREE) More)]

  SELF FREE [%lambda ARGS E] ->
                  (let FV (free-vars [%lambda ARGS E])
                       SELF-VAR (gensym self)
                       [%closure [%lambda [SELF-VAR | ARGS] (convert SELF-VAR FV E)] |
                                (map (convert SELF FREE) FV)])

  SELF FREE [%apply [%lambda PARAMS BODY] | ARGS]  ->
            [%apply [%lambda PARAMS (convert SELF FREE BODY)] | (map (convert SELF FREE) ARGS)]
  SELF FREE [%apply | More] -> [%apply | (map (convert SELF FREE) More)])

(defun closure-convert (ast)
  (convert ignore [] ast ))

(closure-convert [%apply [%lambda [r1235]
        [%if r1235
              y
              z]] a])

