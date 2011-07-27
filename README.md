ocaml-erlang-port
=================

Installation
------------

    $ ocaml setup.ml -configure
    $ ocaml setup.ml -build
    $ ocaml setup.ml -install


Examples
--------

### OCaml

```ocaml
open ErlangTerm

let port_command_dispatcher old_value = function
  | ET_Tuple [ET_Atom "forget"; _] ->
    None, ET_Atom "ok"
  | ET_Tuple [ET_Atom "set"; term] ->
    Some term, ET_Atom "ok"
  | ET_Tuple [ET_Atom "get"; _] ->
    old_value, begin match old_value with
        | None -> ET_Tuple [ET_Atom "error"; ET_Atom "no_value"]
        | Some term -> ET_Tuple [ET_Atom "ok"; term]
    end
  | _ -> raise (Failure "Unknown command")
  ;;

ErlangPort.erlang_port_interact_with_key port_command_dispatcher None
```

To compile the code above run:

    $ cd examples/
    $ ocamlfind ocamlopt -package ocaml-erlang-port -linkpkg -o port_sample port_sample.ml

### Erlang

Make sure you have [mavg](https://github.com/EchoTeam/mavg) package installed,
before launching `portserver.el` -- Erlang part of the example:

	1> c(portserver).
	{ok,portserver}
	2> portserver:start_link({local, ocaml}, "./port_sample").
	{ok,<0.40.0>}
	3> portserver:call(ocaml, get, []).
	{error,no_value}
	4> portserver:call(ocaml, set, {foo,bar}).
	ok
	5> portserver:call(ocaml, get, []).
	{ok,{foo,bar}}
	6> portserver:call(ocaml, forget, []).
	ok
	7> portserver:call(ocaml, get, []).
	{error,no_value}
	8> portserver:ping(ocaml).
	pong
