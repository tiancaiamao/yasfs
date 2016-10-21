package vm

import (
	"encoding/binary"
	"unsafe"

	"github.com/ngaut/log"
)

type ValueType int

const (
	TypeUnit ValueType = iota
	TypeInt
	TypeBool
	TypeTuple
	TypeClosure
	TypeString
	TypeEnv
)

type Value struct {
	ValueType
}

type Unit struct {
	Value
}

type Int struct {
	Value
	v int
}

type Bool struct {
	Value
	v bool
}

type Tuple struct {
	Value
	tag   uint8
	tuple []*Value
}

type Closure struct {
	Value
	env  *Env
	code []byte
	pc   int
}

func (v *Value) Type() ValueType {
	return v.ValueType
}

func NewInt(v int) *Value {
	tmp := Int{v: v}
	tmp.Value.ValueType = TypeInt
	return &tmp.Value
}

func NewBool(v bool) *Value {
	tmp := Bool{v: v}
	tmp.Value.ValueType = TypeBool
	return &tmp.Value
}

func NewTuple(tag uint8, tuple []*Value) *Value {
	tmp := Tuple{tag: tag, tuple: tuple}
	tmp.ValueType = TypeTuple
	return &tmp.Value
}

func NewClosure(pc int, env *Env) *Value {
	tmp := Closure{env: env, pc: pc}
	tmp.ValueType = TypeClosure
	return &tmp.Value
}

func NewEnv(data []*Value) *Value {
	tmp := Env{}
	tmp.data = make([]*Value, len(data))
	copy(tmp.data, data)
	tmp.ValueType = TypeEnv
	return &tmp.Value
}

func (v *Value) Int() *Int {
	return (*Int)(unsafe.Pointer(v))
}

func (v *Value) Bool() *Bool {
	return (*Bool)(unsafe.Pointer(v))
}

func (v *Value) Closure() *Closure {
	return (*Closure)(unsafe.Pointer(v))
}

func (v *Value) Env() *Env {
	return (*Env)(unsafe.Pointer(v))
}

func (v *Value) Tuple() *Tuple {
	return (*Tuple)(unsafe.Pointer(v))
}

type Env struct {
	Value
	data []*Value
}

func (e *Env) Get(n int) *Value {
	return e.data[n]
}

type ctx struct {
	mark  int
	stack []int
	sp    int
}

func (c *ctx) Push(v int) {
	c.stack[c.sp] = v
	c.sp++
}

func (c *ctx) Pop() int {
	c.sp--
	return c.stack[c.sp]
}

type VM struct {
	pc    int
	sp    int
	bp    int
	acc   *Value
	env   *Env
	stack []*Value

	ctx ctx
}

func New() *VM {
	vm := &VM{
		stack: make([]*Value, 200),
		env:   &Env{},
	}
	vm.ctx.stack = make([]int, 100)
	return vm
}

func (vm *VM) Run(code []byte) *Value {
LOOP:
	for {
		switch code[vm.pc] {
		case STOP:
			log.Debug("STOP")
			break LOOP
		case MARK:
			log.Debugf("MARK sp=%d", vm.sp)
			vm.ctx.mark = vm.sp
			vm.pc++
		case CONST:
			v := binary.LittleEndian.Uint32(code[vm.pc+1:])
			log.Debug("CONST", v)
			vm.acc = NewInt(int(v))
			log.Infof("%#v", vm.acc.Int())
			vm.pc += 5
		case ConstTrue:
			vm.acc = NewBool(true)
			vm.pc++
		case ConstFalse:
			vm.acc = NewBool(false)
			vm.pc++
		case MakeTuple:
			size := int(code[vm.pc+2])
			tuple := make([]*Value, size)
			copy(tuple, vm.stack[vm.sp-size:])
			vm.acc = NewTuple(code[vm.pc+1], tuple)
			vm.pc += 3
		case CLOSURE:
			size := binary.LittleEndian.Uint32(code[vm.pc+1:])
			log.Debugf("CLOSURE %d", size)
			vm.acc = NewClosure(vm.pc+5, vm.env.Env())
			vm.pc += int(size + 5)
		case APPLY:
			log.Debug("APPLY")
			cls := vm.acc.Closure()
			// 保存之前的pc
			vm.ctx.Push(vm.pc + 1)
			vm.pc = cls.pc
		case CHECK:
			n := code[vm.pc+1]
			if vm.sp-vm.ctx.mark >= int(n) {
				log.Debugf("CHECK, want args %d, have args %d", n, vm.sp-vm.ctx.mark)
				vm.pc += 2
			} else {
				log.Debug("CHECK partial apply")
				// TODO handle partial apply
				panic("not implement")
			}
		case ENV:
			// copy env to stack
			env := vm.acc.Closure().env
			for _, v := range env.data {
				vm.stack[vm.sp] = v
				vm.sp++
			}
			log.Debugf("ENV, copy %d stack, save bp %d, set bp to sp %d", len(env.data), vm.bp, vm.sp)
			vm.ctx.Push(vm.bp) // 保存bp
			vm.bp = vm.sp - 1
			vm.pc++
		case UNENV:
			vm.bp = vm.ctx.Pop()
			log.Debugf("UNENV, recover bp %d", vm.bp)
			vm.pc++
		// case PUSHADDR:
		// 	size := binary.LittleEndian.Uint32(code[vm.pc+1:])
		// 	vm.stack[vm.sp] = NewReturnAddr(vm.pc + int(size))
		// 	vm.sp++
		// 	vm.stack[vm.sp] = &vm.env.Value
		// 	vm.sp++
		// 	vm.pc += 4
		case ACCESS:
			n := int(code[vm.pc+1])
			vm.acc = vm.stack[vm.bp-n]
			vm.pc += 2
			log.Debugf("ACCESS %d get %v", vm.bp-n, vm.acc)
		case PUSH:
			log.Debugf("PUSH %d %v", vm.sp, vm.acc)
			vm.stack[vm.sp] = vm.acc
			vm.sp++
			vm.pc++
		case ADDINT:
			log.Debug("ADDINT")
			top := vm.stack[vm.sp-1].Int()
			vm.acc.Int().v += top.v
			vm.sp--
			vm.pc++
		case RETURN:
			vm.sp = vm.bp
			vm.pc = vm.ctx.Pop()
			log.Debugf("RETURN sp=%d", vm.sp)
		case JumpIf: // + int
		case Goto: // + int
		case Field: // + int
		case Switch: // + (tag + offset) * n
		default:
			panic("not implement")
		}
	}
	return vm.acc
}
