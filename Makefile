LISP ?= sbcl

.PHONY: build

ci: build

build:
	@echo "Building..."
	$(LISP) --noinform --load './scripts/build.lisp'
