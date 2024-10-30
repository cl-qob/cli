LISP ?= sbcl

LISP_ARGS =	--noinform

.PHONY: build install-ql

ci: build

build:
	@echo "Building..."
	$(LISP) $(LISP_ARGS) --load './scripts/build.lisp'

download-ql:
	@echo "Downloading Quicklisp..."
	curl -O https://beta.quicklisp.org/quicklisp.lisp

install-ql: download-ql
	@echo "Installing Quicklisp..."
	$(LISP) $(LISP_ARGS) --load './quicklisp.lisp' --script './scripts/install-ql.lisp'
