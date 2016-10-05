变量  只不过是给事物命一个名字   环境中第1个 环境中第2个 环境中第47个...
alpha变换，把名字变成在环境中的位置

值 不用管它存在哪里

## fn关键字

f(x) = x + 1
(lambda (x) (+ x 1))
fn x => x + 1
fn x -> x + 1
\x -> x + 1
^x . x + 1
x => x + 1
function (x) { return x + 1; }
lambda x: x + 1
[:x | x + 1]
Function(x) x + 1

M1 M2 M3 ... Mn   ((((M1 M2) M3) ... ) Mn)
必须用括号来表示结合的顺序

匿名函数的写法
fn x -> x + 1

变量绑定的写法，含义是在当前环境里面绑定变量，这不是赋值！
a := 3
a, b, c := 1, 2, 3
计算右值时，左边变量在环境中不可见

## => 与 -> 递归

函数的写法，其实是语法糖
fn f x => x + x
等价于在全局环境中 匿名函数 + 变量绑定
f := fn x -> x + 1

以下代码
fn f x =>
   a := 3
   b := 5
   a+b+x
跟ML代码等的
let f x =
    let a = 3 in
    let b = 5 in
        a + b + x

f := fn x -> x + 1
let f g h =
  g := 3
  h := f g

柯里化
fn x y z -> M
等价于
fn x ->
   fn y ->
      fn z ->
         M

第一期做基本的语法解析、zinc的虚拟机模型、无类型lambda演算
之后加条件 循环 分支
然后是简单类型推导
patten match是需要的，但是第一期暂时不做，至少要做完type infer

fn f x y => 
   x := f x a;
   f x+1 y-1;

没有let in的概念

## ;; 与 ; 以及{}

如果不做分隔会出问题

  fn f n =>
     fn loop i n =>
        if i=n then loop xxx
        else
          yyy // 这个yyy到底是属于loop这一层呢，还是上一层f？

lua用function ... end do ... end这种显示的标记
c Go java 典型的 { }
lisp是()
ML必须有let xxx in 的in来做区分
F#或者python那种用tab缩进来控制是个很坑的事情

缩进的方式可读性好

引入{}

fn x y ->
   fn f1 x y =>
      c := c y;
      f1 (x+1) (y-1);;

fn fact n => if x then
   fn dead => sdf;
   x := fn a b -> b + a;
   dead x;;


fn x -> x + y;

引入;作为end，这很重要

x := (if a
  b
  c)
  5

let loop x =
    loop x+1
    
跟scheme里面的begin是一个概念，函数里面是多个表达式，或者begin里面还多个表达式

(lambda () x1 x2 x3) 等价于 (lambda () (begin x1 x2 x3))
于是只需要支持(begin x1 x2 x3)，在lambda不需要支持多个表达式
我们只是把(begin x1 x2 x3)换成了{x1; x2; x3}


## 类型推导

动态类型灵活，但不安全。我希望偏静态一点。

标注+type check的方式实现起来简单，但类型标注的代码比较啰嗦，写起来不爽。

类型推导坑很深，目前决定还是采用type infer这个feature，但是只支持最简单的推导。

HM系统的问题：

fn id x => x
(fn f -> (f 3), (f true)) id

## 声明

用::符号声明函数

    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
