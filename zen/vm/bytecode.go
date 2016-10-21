package vm

const (
	STOP    = 1
	MARK    = 2
	CONST   = 3
	PUSH    = 4
	CLOSURE = 5
	APPLY   = 6
	CHECK   = 7
	ENV     = 8
	ACCESS  = 9
	ADDINT  = 10
	UNENV   = 11
	RETURN  = 12
	BRANCH  = 13

	ConstTrue
	ConstFalse
	MakeTuple // + tag + size
	TailApply
	JumpIf // + int
	Goto   // + int
	Field  // + int
	Switch // + (tag + offset) * n
)
