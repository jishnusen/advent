LISP ?= sbcl

build:
	$(LISP) --load advent-clisp.asd \
		--eval '(ql:quickload :advent-clisp)' \
		--eval '(asdf:make :advent-clisp)' \
		--eval '(quit)'
