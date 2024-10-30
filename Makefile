LISP ?= sbcl

.PHONY: build install-ql

ci: build

build:
	@echo "Building..."
	$(LISP) --load './scripts/build.lisp'

install-ql:
	@echo "Installing Quicklisp..."
	$(LISP) --load './scripts/quicklisp.lisp' --load './scripts/install-ql.lisp'
