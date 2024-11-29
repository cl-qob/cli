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
	rm './quicklisp.lisp'

install-ql-no-network:
	@echo "Installing Quicklisp..."
	$(LISP) $(LISP_ARGS) --load './scripts/quicklisp.lisp' --script './scripts/install-ql.lisp'

build-nix: install-ql-no-network build

command-global:
	./test/commands/global/run.sh

command-local:
	./test/commands/local/run.sh
