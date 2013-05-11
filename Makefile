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

clean-ocaml:
	cd ocaml-erlang-port; $(MAKE) clean

check: ocaml-check
	$(REBAR) eunit

clean: clean-ocaml
	$(REBAR) clean

.PHONY: ocaml clean-ocaml ocaml-check

REBAR ?= $(shell which ./rebar)
