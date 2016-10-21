int是32位或者64位，由机器字长决定

## 寄存器

pc
bp
sp
acc
env
global

## 指令集

CLOSURE ofst 创建闭包，pc为下一个，环境为空
APPLY 将pc和env设置为acc(闭包)里面的值
ENV n 将参数数量进栈，更新bp，保存bp
UNENV 跟ENV相反的操作
RETURN n 出栈n个元素，将pc和env设置为acc里面的值
MARK  标记设置为sp
ACC n 栈顶往下第n个到acc

ACC0 栈顶到acc
PUSH0 acc到栈顶
PUSHACC n stack[top-n]->acc
POP n 出栈n个
ASSIGN n 栈第n个设置为acc，acc设置为unit
ENVACC n acc设置为env第n个

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

其实"环境"由静态跟动态两个部分组成，动态的那部分保存在栈上面，静态的部分保存在env寄存器里面
bp跟stack[bp]区间，是动态环境

闭包 = 环境 + pc
环境 = 动态环境 + 静态环境
动态环境 = bp跟stack[bp]区间

闭包跟上下文其实等价

函数调用就是保存上下文，切换到新的上下文
保存上下文跟切换其实是两个动作，保存上下文涉及保存旧的pc,env,bp；切换上下文涉及覆盖当前的pc,env,bp；
对于尾递归，可以不用保存上下文，直接切换新的上下文
所以呢，相关的指令是三条：
PUSHADDR 保存上下文 -- 将bp env pc进栈
APPLY 切换上下文 -- bp更新为sp、pc和env更新为acc里面的值
RETURN 返回之前的上下文

闭包的形成有以下几种情况：
CLOSURE指令：非尾调用的时候lambda会生成闭包
GRAB指令：当判断参数不够时，会生成闭包
PUSHADDR指令：返回信息实际上就是闭包

仅有一个bp是不够的，需要加一个mark
mark不需要保存在栈内(不需要恢复)，但是需要有
bp还是原始的作用
