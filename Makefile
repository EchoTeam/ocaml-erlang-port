all:
	$(REBAR) compile

install:
	cd ocaml_src; $(MAKE) install

install-package:
	cd ocaml_src; $(MAKE) install-package

uninstall:
	cd ocaml_src; $(MAKE) uninstall

ocaml_check:
	cd ocaml_src; $(MAKE) check

ocaml_src:
	cd ocaml_src; $(MAKE)

ocaml_src_clean:
	cd ocaml_src; $(MAKE) clean

check: ocaml_check

clean: ocaml_src_clean
	$(REBAR) clean

.PHONY: ocaml_src ocaml_src_clean ocaml_src_clean

REBAR ?= $(shell which ./rebar)
