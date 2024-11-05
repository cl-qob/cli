LISP ?= sbcl

LISP_ARGS =	--noinform

.PHONY: build build-deploy download-ql install-ql

ci: build

build:
	@echo "Building..."
	$(LISP) $(LISP_ARGS) --script './scripts/build.lisp'

build-deploy:
	@echo "Building... (with Deploy)"
	$(LISP) $(LISP_ARGS) --script './scripts/deploy.lisp'

download-ql:
	@echo "Downloading Quicklisp..."
	curl -O https://beta.quicklisp.org/quicklisp.lisp

install-ql: download-ql
	@echo "Installing Quicklisp..."
	$(LISP) $(LISP_ARGS) --load './quicklisp.lisp' --script './scripts/install-ql.lisp'
