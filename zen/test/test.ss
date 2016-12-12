;; identity
((lambda (x) x) 5)
5

;; multi argument
((lambda (x y) x) 2 42)
2

;; multi argument
((lambda (x y) y) 2 42)
42

;; partial apply
(((lambda (x y) y) 1) 42)
42

;; variable bind
(let ((a 3)) a)
3

;; plus
(+ 1 2)
3

((lambda (x y) (+ x y)) 3 5)
8

;; mul
(* 3 7)
21

;; if then else
(if (= 1 1) 5 2)
5

;; closure
((lambda (x)
  (lambda (y)
    (+ x y)))
 1 2)
3

;; sum 100
((lambda1 (loop i sum)
          (if (= i 101)
              sum
              (loop (+ i 1) (+ sum i))))
 1 0)
5050

;; fact 5
((lambda1 (fact n)
          (if (= n 0)
              1
              (* n (fact (- n 1)))))
 5)
120

;; order
((lambda (a b)
  (let ((x 3)
        (y 4))
    x)) 1 2)
3

((lambda (a b)
   (let ((x 3)
         (y 4))
     y)) 1 2)
4

((lambda (a b)
   (let ((x 3)
         (y 4))
     a)) 1 2)
1

((lambda (a b)
   (let ((x 3)
         (y 4))
     b)) 1 2)
2
