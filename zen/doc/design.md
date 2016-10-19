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
还是引入let in

Y (fn fact ->
      fn n ->
          if n=0 then 1 else n*(fact (n-1)))
          
等价于

fn fact n => if n=0 then 1 else n*(fact (n-1))

用let来导出
let fact = fn fact n => ... in
等价于
let fn fact n => ... in

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

做type infer全部转化成带类型的lambda演算了再做，可以简化问题

HM系统的问题：

fn id x => x
(fn f -> (f 3), (f true)) id

## 声明 模块

用::符号声明函数

    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b

类型标注用[]括号包起来
fn id:[Bool->Bool] x:[Bool] => x

加一层，从sugar层到typed lambda演算层，typed lambda演算完全使用TAPL的

## 泛型

不支持泛型，泛型给语法带来的复杂性大于它的实用价值

我认为几乎只有在容器类型数据结构的时候需要用到泛型，引入了过多的复杂性，增加了实现难度，为此是不值得的

泛型必须有，没有什么都表达不了

[true, true, false, true] 这个类型是list<bool>
[2, 3, 7, 8, 12] 这个类型是list<int>
some 5 跟some true 是some对不同int和bool的实例化

还是不要支持泛型...太复杂了

## 元组，记录，以及Union类型

元组 类型记法：
(Int, Bool, String)
值记法：
(1, true, "xxx")
访问方法
(1, true, "xxx").0 = 1
(1, true, "xxx").1 = true
(1, true, "xxx").2 = "xxx"

记录是带名字的元组，其它语言中的结构体
类型记法：
type T struct {
    field1 Int,
    field2 Bool,
    field3 String,
}
值记法：
T{field1=1,
field2=true,
field3="xxx"}

元组是记录的简化，等价于
(Int, Bool, String) == T{0:Int, 1:Bool, 2: String}

union类型是有价值的
类型记法：
type option union {
    Some int,
    None unit,
}
值记法：
option{Some=3}

union类型引入以及需要增加模式匹配。
x = option{Some=3}
switch x as option {
    case Some a: 3
    case None: 5
}
这样子设计，从switch表达式就可以推断出x的类型是option，就像 + 直接推断出两边是int类型

ocaml的match with语法设计有点缺陷，因为没有结束标记，嵌套时会有麻烦，需要加括号

match x with 
| Some a ->
    (match x with 
    | _ -> true)
| None -> false

不加括号就没法区分 | None -> false是属于哪个match的，还是显示的用{}比较好

# tagged tuple

// tag是很特殊的东西，它的**值**要参与到类型推导
tagged tuple?

? -> ? -> #CONS
fn cons (x, y) { #CONS(x, y) }
fn null () { #NULL() }

car :: (?) -> ?
fn car (x) { field 0 x }

some :: ? -> #SOME(?)
fn some (x) { #SOME(x) }
fn unit (x) { #UNIT(X) }

能否推出x是 tuple 长度大于1 第一项是42 ?
car被具体化为 (?) -> int
x被具体化为 (int)
if car x = 42 
x :: (int)
if field 1 x = 42

(tagged tuple len>=2, ?) -> ?
fn cdr (x) { field 1 x }

cons 3 (cons true (cons 1 #NULL()))

#CONS(3, #CONS(true, #CONS(1, #NULL())))

(NULL or CONS, ?, ?) -> INT -> INT
fn length (ls, sum) {
    switch ls {
      case NULL: sum
      case CONS: length (cdr ls) (sum+1)
    }
}

fn tree_node (x, y) { #TreeNode(x, y) }
fn leaf (x) { #Leaf(x) }

tuple其实是tag tuple的特例嘛，那么只需要后者足已。

## FFI

参考lua，不可以让C引用到虚拟机里面的任何对象！

语言的对象只能在语言中访问。C中想访问，只是拷贝一份，修改，再弄回去。

这样的原因是，一旦暴露类似没有Value类型的东西给C，C就有可能将它存储起来。
而虚拟机那边的GC并不知道对象在外部系统中被引用，可能删除该对象，在C那边就会出现悬挂引用

```
value bar(value list) {
    CAMLparam l(list);
    CAMLlocal l(temp);
    temp = alloc_tuple(2); // 这里的alloc可能触发GC，如果没有前面保护list的动作，list可能已经被释放
    CAMLreturn (Val_unit);
} 
```

通过一个栈交互
