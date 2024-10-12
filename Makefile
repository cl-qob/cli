LISP ?= sbcl

.PHONY: build

ci: build

build:
	@echo "Building..."
	$(LISP)	--load './scripts/build.lisp'
