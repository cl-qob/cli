LISP ?= sbcl

LISP_ARGS =	--noinform

.PHONY: build install-ql

ci: build

build:
	@echo "Building..."
	$(LISP) $(LISP_ARGS) --load './scripts/build.lisp'

install-ql:
	@echo "Installing Quicklisp..."
	$(LISP) $(LISP_ARGS) --load './scripts/quicklisp.lisp' --script './scripts/install-ql.lisp'
