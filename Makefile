all:
	$(REBAR) compile

install:
	cd ocaml-erlang-port; $(MAKE) install

install-package:
	cd ocaml-erlang-port; $(MAKE) install-package

uninstall:
	cd ocaml-erlang-port; $(MAKE) uninstall

ocaml-check:
	cd ocaml-erlang-port; $(MAKE) check

ocaml:
	cd ocaml-erlang-port; $(MAKE)

ocaml-clean:
	cd ocaml-erlang-port; $(MAKE) clean

check: ocaml-check

clean: ocaml-clean
	$(REBAR) clean

.PHONY: ocaml ocaml-clean ocaml-check

REBAR ?= $(shell which ./rebar)
