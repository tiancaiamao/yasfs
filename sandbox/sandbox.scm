(module sandbox
	(make-sandbox
	 sandbox-ref
	 sandbox-set!
	 sandbox-remove!
	 sandbox-eval)
	
	(import scheme chicken)

	(include "desugar.scm")
	(include "eval.scm")
	)
