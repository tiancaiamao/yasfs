package vm

import (
	"bytes"
	"encoding/binary"
)

type Emit struct {
	bytes.Buffer
}

func (e *Emit) CONST(v int32) {
	e.WriteByte(CONST)
	binary.Write(&e.Buffer, binary.LittleEndian, v)
}

func (e *Emit) PUSH() {
	e.WriteByte(PUSH)
}

func (e *Emit) CLOSURE(c []byte) {
	e.WriteByte(CLOSURE)
	binary.Write(&e.Buffer, binary.LittleEndian, int32(len(c)))
	e.Write(c)
}

func (e *Emit) CHECK(n byte) {
	e.WriteByte(CHECK)
	e.WriteByte(n)
}

func (e *Emit) ACCESS(n byte) {
	e.WriteByte(ACCESS)
	e.WriteByte(n)
}

func (e *Emit) ADDINT() {
	e.WriteByte(ADDINT)
}

func (e *Emit) RETURN() {
	e.WriteByte(RETURN)
}

func (e *Emit) APPLY() {
	e.WriteByte(APPLY)
}

func (e *Emit) STOP() {
	e.WriteByte(STOP)
}

func (e *Emit) MARK() {
	e.WriteByte(MARK)
}

func (e *Emit) ENV() {
	e.WriteByte(ENV)
}

func (e *Emit) UNENV() {
	e.WriteByte(UNENV)
}
