# If nothing works, try `ocamlmklib -o $(PACKAGE) *.ml`

PACKAGE=ocaml-erlang-port
LIBSRCS=ErlangTerm.ml ErlangPort.ml
LIBCMIS=${LIBSRCS:.ml=.cmi}
LIBCMOS=${LIBSRCS:.ml=.cmo}
LIBCMXS=${LIBSRCS:.ml=.cmx}

LIBS=$(PACKAGE).cma $(PACKAGE).cmxa $(PACKAGE).a lib$(PACKAGE).a

all: $(LIBCMIS) $(LIBS)

install:
	@echo "Use install-package if you want to do a system-wide install"

install-package: uninstall $(LIBCMIS) $(LIBS)
	ocamlfind install $(PACKAGE) $(LIBS) $(LIBCMIS) META

uninstall:
	ocamlfind remove $(PACKAGE)

$(PACKAGE).cma: $(LIBCMIS) $(LIBCMOS)
	ocamlmklib -o $(PACKAGE) $(LIBCMOS)

$(PACKAGE).cmxa: $(LIBCMIS) $(LIBCMXS)
	ocamlmklib -o $(PACKAGE) $(LIBCMXS)

lib$(PACKAGE).a: $(LIBCMIS) $(LIBCMXS)
	ocamlmklib -o lib$(PACKAGE) $(LIBCMXS)

check: erlterm_check
	@echo "Running embedded self-check"
	./erlterm_check
	@echo "Running external tests"
	./erlterm_check ./erlterm_tests/*.et
	@echo "make check: OK"

erlterm_check: $(LIBCMXS) ErlangTerm_Check.cmx
	ocamlfind ocamlopt -package num -linkpkg -o erlterm_check $(LIBCMXS) ErlangTerm_Check.cmx

.SUFFIXES: .ml .mli .cmx .cmo .cmi

.ml.cmx:
	ocamlfind ocamlopt -o $@ -c $<

.ml.cmo:
	ocamlfind ocamlc -o $@ -c $<

.mli.cmi:
	ocamlfind ocamlc -o $@ $<

clean:
	rm -f *.cm* *.[ao]
