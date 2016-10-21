package vm

import (
	"testing"
)

func TestBasic(t *testing.T) {
	// (fn x y -> x+y) 1 2
	c := &Emit{}
	c.CHECK(2)
	c.ENV()
	c.ACCESS(0)
	c.PUSH()
	c.ACCESS(1)
	c.ADDINT()
	c.UNENV()
	c.RETURN()

	d := &Emit{}
	d.MARK()
	d.CONST(2)
	d.PUSH()
	d.CONST(1)
	d.PUSH()
	d.CLOSURE(c.Bytes())
	d.APPLY()
	d.STOP()

	vm := New()
	r := vm.Run(d.Bytes())
	if r.ValueType != TypeInt || r.Int().v != 3 {
		t.Errorf("require 3, get %#v", r)
	}
}
