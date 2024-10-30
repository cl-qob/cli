LISP ?= sbcl

.PHONY: build install-ql

ci: build

build:
	@echo "Building..."
	$(LISP) --noinform --load './scripts/build.lisp'

install-ql:
	@echo "Installing Quicklisp..."
	$(LISP) --noinform --load './scripts/quicklisp.lisp' --load './scripts/install-ql.lisp'
