int是32位或者64位，由机器字长决定

## 寄存器

pc
bp
sp
accu
env
global


## 指令集


ACC0 栈顶到acc
ACC n 栈顶往下第n个到acc
PUSH0 acc到栈顶
PUSHACC n stack[top-n]->acc
POP n 出栈n个
ASSIGN n 栈第n个设置为acc，acc设置为unit
ENVACC n acc设置为env第n个
PUSHADDR ofst 进栈环境，进栈pc+ofst
APPLY 将pc和env设置为acc(闭包)里面的值
RETURN n 出栈n个元素，将pc和env设置为acc里面的值
GRAB n 如果参数足够(args大于等于n)，直接将args置为0；否则创建闭包(pc-3,环境,栈上其它..)  

GRAB语义是栈到环境

CLOSURE n ofst 将acc以及栈顶往下n个，连同pc+ofst，创建闭包
OFFSETCLOSURE n 将acc设置为环境中第n个闭包值
GETGLOBAL n 将accu设置为global数据的第n个
GETGLOBALFIELD n p global的第n个的第p个field
SETGLOBAL n 将global的第n个设置为accu，然后accu设置为unit
MAKEBLOCK n t 用accu和栈上n-1个元素，创建block，tag为t，将block设置到accu
GETFIELD0 将accu设置为accu的field0元素
GETFIELD n 将accu设置为accu的field n元素
SETFIELD n 将accu对就block里面的第n个field设置为栈顶值，出栈，accu设置为unit
BRANCH ofst 将pc加ofst
BRANCHIF ofst 如果accu不为0，则将pc加ofst
SWITCH n tab 
BOOLNOT 对accu执行not操作
CCALL p 保存环境，调用p，accu设置为返回值，恢复p
CONST n 将accu设置为常量n
NEGINT 对accu执行取负
ADDINT
SUBINT
MULINT
DIVINT
MODINT
ANDINT
ORINT
XORINT
LSLINT
LSRINT
EQ
NEQ
LTINT
LEINT
GTINT
GEINT
OFFSETINT ofst 在accu加上ofst
STOP
