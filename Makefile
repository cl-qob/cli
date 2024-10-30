LISP ?= sbcl

.PHONY: build

ci: build

build:
	@echo "Building..."
	$(LISP) --noinform --load './scripts/build.lisp'

install-ql:
	@echo "Installing Quicklisp..."
	$(LISP) --noinform --load './scripts/install-ql.lisp'
