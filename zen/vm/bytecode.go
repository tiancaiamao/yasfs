package vm

const (
	STOP = iota
	MARK
	CONST
	PUSH
	CLOSURE
	APPLY
	CHECK
	ENV
	ACCESS
	ADDINT
	UNENV
	RETURN
	BRANCH

	ConstTrue
	ConstFalse
	MakeTuple // + tag + size
	TailApply
	JumpIf // + int
	Goto   // + int
	Field  // + int
	Switch // + (tag + offset) * n
)
